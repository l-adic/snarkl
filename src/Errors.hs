{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Errors where

import Control.Exception
import Data.Typeable

newtype ErrMsg = ErrMsg {errMsg :: String}
  deriving (Typeable)

instance Show ErrMsg where
  show (ErrMsg msg) = msg

instance Exception ErrMsg

fail_with :: ErrMsg -> a
fail_with e = throw e
