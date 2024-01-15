module Snarkl.CLI.Utils where

import qualified Data.ByteString.Lazy as LBS
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

createDirForFile :: FilePath -> IO ()
createDirForFile filePath =
  createDirectoryIfMissing True (takeDirectory filePath)

writeFile ::
  FilePath ->
  LBS.ByteString ->
  IO ()
writeFile filePath contents = do
  createDirForFile filePath
  LBS.writeFile filePath contents
