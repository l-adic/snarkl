module Data.JSONLines
  ( ToJSONLines (..),
    FromJSONLines (..),
    JSONLine (..),
    WithHeader (..),
    NoHeader (..),
  )
where

import qualified Data.Aeson as A
import Data.ByteString.Builder (Builder, lazyByteString, toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Kind (Type)
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

data WithHeader :: Type -> Type -> Type where
  WithHeader :: hdr -> [item] -> WithHeader hdr item

newtype NoHeader item = NoHeader [item]

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

class ToJSONLines a where
  toJSONLines :: a -> LBS.ByteString

instance (A.ToJSON item) => ToJSONLines (NoHeader item) where
  toJSONLines (NoHeader items) = toBS . jsonlBuilder $ items

instance {-# OVERLAPPABLE #-} (A.ToJSON a) => ToJSONLines [a] where
  toJSONLines = toJSONLines . NoHeader

instance (A.ToJSON hdr, A.ToJSON item) => ToJSONLines (WithHeader hdr item) where
  toJSONLines (WithHeader hdr items) = toBS $ jsonLine hdr <> jsonlBuilder items

class FromJSONLines a where
  fromJSONLines :: [LazyByteString] -> Either String a

instance (A.FromJSON item) => FromJSONLines (NoHeader item) where
  fromJSONLines items = NoHeader <$> traverse A.eitherDecode items

instance {-# OVERLAPPABLE #-} (A.FromJSON a) => FromJSONLines [a] where
  fromJSONLines = fmap (\(NoHeader x) -> x) . fromJSONLines

instance (A.FromJSON hdr, A.FromJSON item) => FromJSONLines (WithHeader hdr item) where
  fromJSONLines (h : is) = do
    hdr <- A.eitherDecode h
    items <- traverse A.eitherDecode is
    pure $ WithHeader hdr items
  fromJSONLines [] = Left "Empty file"
