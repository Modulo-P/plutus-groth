
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Groth16 where

import           PlutusTx         as Tx
import           PlutusTx.Prelude
import qualified PlutusTx.Show    as Tx

data VerificationKey = VerificationKey {
    nPublic     :: Integer
  , vkAlpha     :: [Integer]
  , vkBeta      :: [[Integer]]
  , vkGamma     :: [[Integer]]
  , vkDelta     :: [[Integer]]
  , vkAlphaBeta :: [[[Integer]]]
  , vkIC        :: [[Integer]]
  }

Tx.deriveShow ''VerificationKey

data Proof = Proof {
    piA :: [Integer]
  , piB :: [[Integer]]
  , piC :: [Integer]
  }

