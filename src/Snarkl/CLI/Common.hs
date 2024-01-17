module Snarkl.CLI.Common where

import qualified Data.ByteString.Lazy as LBS
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

writeFileWithDir ::
  FilePath ->
  LBS.ByteString ->
  IO ()
writeFileWithDir filePath contents = do
  createDirectoryIfMissing True (takeDirectory filePath)
  LBS.writeFile filePath contents

readLines ::
  FilePath ->
  IO [LBS.ByteString]
readLines filePath = do
  contents <- LBS.readFile filePath
  return $ LBS.split 0xa contents

mkR1CSFilePath :: FilePath -> String -> FilePath
mkR1CSFilePath rootDir name = rootDir <> "/" <> name <> "-r1cs.jsonl"

mkWitnessFilePath :: FilePath -> String -> FilePath
mkWitnessFilePath rootDir name = rootDir <> "/" <> name <> "-witness.jsonl"

mkInputsFilePath :: FilePath -> String -> FilePath
mkInputsFilePath rootDir name = rootDir <> "/" <> name <> "-inputs.jsonl"

mkConstraintsFilePath :: FilePath -> String -> FilePath
mkConstraintsFilePath rootDir name = rootDir <> "/" <> name <> "-constraints.jsonl"
