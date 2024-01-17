module Test.ArkworksBridge where

import qualified Data.ByteString.Lazy as LBS
import Data.Field.Galois (GaloisField, PrimeField)
import Data.JSONLines (NoHeader (NoHeader), ToJSONLines (toJSONLines), WithHeader (WithHeader))
import qualified Data.Map as Map
import Data.Typeable (Typeable)
import Snarkl.Backend.R1CS
import Snarkl.Backend.R1CS.R1CS (witnessInputs)
import Snarkl.CLI.Common (mkInputsFilePath, mkR1CSFilePath, mkWitnessFilePath)
import Snarkl.Common (Assgn (Assgn), FieldElem (FieldElem))
import Snarkl.Compile (SimplParam, compileCompToR1CS)
import Snarkl.Language (Comp)
import Snarkl.Toplevel (wit_of_cs)
import qualified System.Exit as GHC
import System.Process (createProcess, shell, waitForProcess)

data CMD k where
  CreateTrustedSetup :: (Typeable ty, GaloisField k) => FilePath -> String -> [SimplParam] -> Comp ty k -> CMD k
  CreateProof :: (Typeable ty, GaloisField k) => FilePath -> String -> [SimplParam] -> Comp ty k -> [k] -> CMD k
  RunR1CS :: (Typeable ty, GaloisField k) => FilePath -> String -> [SimplParam] -> Comp ty k -> [k] -> CMD k

runCMD :: (PrimeField k) => CMD k -> IO GHC.ExitCode
runCMD (CreateTrustedSetup rootDir name simpl c) = do
  let (r1cs, _) = compileCompToR1CS simpl c
      r1csFilePath = mkR1CSFilePath rootDir name
  LBS.writeFile r1csFilePath . toJSONLines $
    WithHeader (r1csHeader r1cs) (r1cs_clauses r1cs)
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
  let (r1cs, simplifiedCS) = compileCompToR1CS simpl c
      wit@(Witness {witness_assgn = Assgn m}) = wit_of_cs inputs simplifiedCS
      r1csFilePath = mkR1CSFilePath rootDir name
      witsFilePath = mkWitnessFilePath rootDir name
  LBS.writeFile r1csFilePath . toJSONLines $
    WithHeader (r1csHeader r1cs) (r1cs_clauses r1cs)
  LBS.writeFile witsFilePath . toJSONLines $
    WithHeader (witnessHeader wit) (Map.toList (FieldElem <$> m))
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
  let (r1cs, simplifiedCS) = compileCompToR1CS simpl c
      wit@(Witness {witness_assgn = Assgn m}) = wit_of_cs inputs simplifiedCS
      r1csFilePath = mkR1CSFilePath rootDir name
      witsFilePath = mkWitnessFilePath rootDir name
      inputsFilePath = mkInputsFilePath rootDir name
  LBS.writeFile r1csFilePath . toJSONLines $
    WithHeader (r1csHeader r1cs) (r1cs_clauses r1cs)
  LBS.writeFile witsFilePath . toJSONLines $
    WithHeader (witnessHeader wit) (Map.toList (FieldElem <$> m))
  let Assgn inputAssignments = witnessInputs wit
  LBS.writeFile inputsFilePath . toJSONLines $
    NoHeader (Map.toList $ FieldElem <$> inputAssignments)
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
