module Test.ArkworksBridge where

import qualified Data.ByteString.Lazy as LBS
import Data.Field.Galois (GaloisField, PrimeField)
import Data.JSONLines (ToJSONLines (toJSONLines))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Typeable (Typeable)
import Debug.Trace (trace)
import Snarkl.AST (Comp)
import Snarkl.Backend.R1CS
import Snarkl.Backend.R1CS.R1CS (witnessInputs)
import Snarkl.CLI.Common (mkInputsFilePath, mkR1CSFilePath, mkWitnessFilePath)
import Snarkl.Common (Assgn (Assgn), FieldElem (FieldElem), InputAssignment (OutputAssignment, PublicInputAssignment), Var (Var))
import Snarkl.Compile (SimplParam, compileCompToR1CS)
import Snarkl.Constraint (ConstraintSystem (cs_out_vars, cs_public_in_vars), SimplifiedConstraintSystem (unSimplifiedConstraintSystem))
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import Snarkl.Toplevel (wit_of_cs)
import qualified System.Exit as GHC
import System.Process (createProcess, shell, waitForProcess)

data CMD k where
  CreateTrustedSetup :: (Typeable ty, GaloisField k) => FilePath -> String -> [SimplParam] -> Comp ty k -> CMD k
  CreateProof :: (Typeable ty, GaloisField k) => FilePath -> String -> [SimplParam] -> Comp ty k -> [k] -> CMD k
  RunR1CS :: (Typeable ty, GaloisField k) => FilePath -> String -> [SimplParam] -> Comp ty k -> [k] -> CMD k

runCMD :: (PrimeField k) => CMD k -> IO GHC.ExitCode
runCMD (CreateTrustedSetup rootDir name simpl c) = do
  let (r1cs, _, _) = compileCompToR1CS simpl c
      r1csFilePath = mkR1CSFilePath rootDir name
  LBS.writeFile r1csFilePath $ toJSONLines r1cs
  let cmd =
        mkCommand
          "create-trusted-setup"
          [ ("r1cs", r1csFilePath),
            ("verifying-key", mkVerifyingKeyFilePath rootDir name),
            ("proving-key", mkProvingKeyFilePath rootDir name)
          ]

  (_, _, _, hdl) <- createProcess $ shell cmd
  waitForProcess hdl
runCMD (CreateProof rootDir name simpl c inputs) = do
  let (r1cs, simplifiedCS, _) = compileCompToR1CS simpl c
  let public_in_vars = cs_public_in_vars (unSimplifiedConstraintSystem simplifiedCS)
      witness = wit_of_cs inputs Map.empty simplifiedCS
      r1csFilePath = mkR1CSFilePath rootDir name
      witsFilePath = mkWitnessFilePath rootDir name
  LBS.writeFile r1csFilePath $ toJSONLines r1cs
  LBS.writeFile witsFilePath $ toJSONLines witness
  let cmd =
        mkCommand
          "create-proof"
          [ ("witness", witsFilePath),
            ("r1cs", r1csFilePath),
            ("proof", mkProofFilePath rootDir name),
            ("proving-key", mkProvingKeyFilePath rootDir name)
          ]
  (_, _, _, hdl) <- createProcess $ shell cmd
  waitForProcess hdl
runCMD (RunR1CS rootDir name simpl c inputs) = do
  let (r1cs, simplifiedCS, _) = compileCompToR1CS simpl c
      [out] = cs_out_vars (unSimplifiedConstraintSystem simplifiedCS)
      wit@(Witness {witness_assgn = Assgn m}) = wit_of_cs inputs Map.empty simplifiedCS
      outVal = case Map.lookup out m of
        Nothing ->
          failWith $
            ErrMsg
              ( "output variable "
                  ++ show out
                  ++ "not mapped, in\n  "
                  ++ show wit
              )
        Just v -> v
  let r1csFilePath = mkR1CSFilePath rootDir name
      witsFilePath = mkWitnessFilePath rootDir name
      inputsFilePath = mkInputsFilePath rootDir name
  LBS.writeFile r1csFilePath $ toJSONLines r1cs
  LBS.writeFile witsFilePath $ toJSONLines wit
  let is =
        let ls = trace (show outVal) $ Map.toList $ Map.delete out m
         in -- TODO (fix this hack)
            zipWith PublicInputAssignment (Var <$> [1 ..]) inputs <> [OutputAssignment out outVal]
  LBS.writeFile inputsFilePath $ toJSONLines is
  let cmd =
        mkCommand
          "run-r1cs"
          [ ("inputs", inputsFilePath),
            ("witness", witsFilePath),
            ("r1cs", r1csFilePath)
          ]
  (_, _, _, hdl) <- createProcess $ shell cmd
  waitForProcess hdl

mkCommand :: String -> [(String, FilePath)] -> String
mkCommand baseCMD kvs =
  let args = unwords $ map (\(k, v) -> "--" <> k <> " " <> v) kvs
   in "arkworks-bridge " <> baseCMD <> " " <> args

mkProofFilePath :: FilePath -> String -> FilePath
mkProofFilePath rootDir name = rootDir <> "/" <> name <> "-proof"

mkProvingKeyFilePath :: FilePath -> String -> FilePath
mkProvingKeyFilePath rootDir name = rootDir <> "/" <> name <> "-pk"

mkVerifyingKeyFilePath :: FilePath -> String -> FilePath
mkVerifyingKeyFilePath rootDir name = rootDir <> "/" <> name <> "-vk"
