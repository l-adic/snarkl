module Data.JSONLines
  ( ToJSONLines (..),
    FromJSONLines (..),
    JSONLine (..),
    NoHeader (..),
    jsonlBuilder,
    jsonLine,
  )
where

import qualified Data.Aeson as A
import Data.ByteString.Builder (Builder, lazyByteString, toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.String.Conversions (LazyByteString)

data JSONLine
  = JSONLine Builder
  | EmptyLine

instance Semigroup JSONLine where
  EmptyLine <> a = a
  a <> EmptyLine = a
  JSONLine a <> JSONLine b = JSONLine (a <> "\n" <> b)

instance Monoid JSONLine where
  mempty = EmptyLine

data NoHeader = NoHeader

jsonlBuilder ::
  (Foldable t) =>
  (A.ToJSON a) =>
  t a ->
  JSONLine
jsonlBuilder = foldMap jsonLine

jsonLine :: (A.ToJSON a) => a -> JSONLine
jsonLine = JSONLine . lazyByteString . A.encode

toBS :: JSONLine -> LBS.ByteString
toBS EmptyLine = mempty
toBS (JSONLine a) = toLazyByteString a

class ToJSONLines hdr item where
  toJSONLines :: (Foldable t) => hdr -> t item -> LBS.ByteString

instance (A.ToJSON item) => ToJSONLines NoHeader item where
  toJSONLines _ = toBS . jsonlBuilder

instance {-# OVERLAPPABLE #-} (A.ToJSON hdr, A.ToJSON item) => ToJSONLines hdr item where
  toJSONLines hdr items = toBS $ jsonLine hdr <> jsonlBuilder items

class FromJSONLines hdr item where
  fromJSONLines :: [LazyByteString] -> Either String (hdr, [item])

instance (A.FromJSON item) => FromJSONLines NoHeader item where
  fromJSONLines items = (NoHeader,) <$> traverse A.eitherDecode items

instance {-# OVERLAPPABLE #-} (A.FromJSON hdr, A.FromJSON item) => FromJSONLines hdr item where
  fromJSONLines (hdr : items) = do
    hdr' <- A.eitherDecode hdr
    items' <- traverse A.eitherDecode items
    pure (hdr', items')
  fromJSONLines [] = Left "Empty file"
