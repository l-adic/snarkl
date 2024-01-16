module Snarkl.Utils
  ( writeFile,
    writeJSONLines,
    readJSONLines,
  )
where

import qualified Data.Aeson as A
import Data.ByteString.Builder (Builder, lazyByteString)
import qualified Data.ByteString.Builder as LBS
import qualified Data.ByteString.Lazy as LBS
import Data.Data (Proxy)
import Snarkl.Errors (ErrMsg (ErrMsg), failWith)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Prelude hiding (writeFile)

writeFile ::
  FilePath ->
  LBS.ByteString ->
  IO ()
writeFile filePath contents = do
  createDirectoryIfMissing True (takeDirectory filePath)
  LBS.writeFile filePath contents

writeJSONLines ::
  (Foldable t) =>
  (Functor t) =>
  (A.ToJSON hdr) =>
  (A.ToJSON a) =>
  FilePath ->
  Maybe hdr ->
  t a ->
  IO ()
writeJSONLines filePath hdr items =
  let contents = jsonlBuilder items
      b = case hdr of
        Nothing -> contents
        Just h -> jsonLine h <> contents
   in writeFile
        filePath
        (LBS.toLazyByteString $ unJSONLine b)

readJSONLines ::
  (A.FromJSON a, A.FromJSON hdr) =>
  FilePath ->
  Maybe (Proxy hdr) ->
  IO (Maybe hdr, [a])
readJSONLines filePath (mHdrType :: Maybe (Proxy hdr)) = do
  file <- LBS.readFile filePath
  let ls = LBS.split 0x0a file
  case ls of
    [] -> failWith $ ErrMsg "Empty file"
    (a : as) -> either (failWith . ErrMsg) pure $ case mHdrType of
      Nothing -> do
        items <- traverse A.eitherDecode (a : as)
        pure (Nothing, items)
      Just _ -> do
        hdr <- Just <$> A.eitherDecode @hdr a
        items <- traverse A.eitherDecode as
        pure (hdr, items)

newtype JSONLine = JSONLine {unJSONLine :: Builder}

instance Semigroup JSONLine where
  JSONLine a <> JSONLine b = JSONLine (a <> "\n" <> b)

jsonLine :: (A.ToJSON a) => a -> JSONLine
jsonLine = JSONLine . lazyByteString . A.encode

jsonlBuilder :: (Foldable t, Functor t, A.ToJSON a) => t a -> JSONLine
jsonlBuilder = foldl1 (<>) . fmap jsonLine
