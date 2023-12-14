module Snarkl.Errors where

import Control.Exception
import Data.String (IsString)
import Data.Typeable

newtype ErrMsg = ErrMsg {errMsg :: String}
  deriving (Typeable, IsString)

instance Show ErrMsg where
  show (ErrMsg msg) = msg

instance Exception ErrMsg

failWith :: ErrMsg -> a
failWith = throw
