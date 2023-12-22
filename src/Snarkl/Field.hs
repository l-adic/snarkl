{-# OPTIONS_GHC -fno-warn-orphans #-}

module Snarkl.Field where

import Data.Field.Galois (Prime, PrimeField (fromP))
import Prettyprinter (Pretty (..))

type P_BN128 = 21888242871839275222246405745257275088548364400416034343698204186575808495617

type F_BN128 = Prime P_BN128

instance Pretty F_BN128 where
  pretty = pretty . fromP
