
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Groth16 where

import           BLS12381         (AffinePoint (..), Fq1, Fq2 (..),
                                   FromInteger (fromInteger), JPoint (JPoint),
                                   affineToJPoint, jpointMul, jpointToAffine,
                                   pairing)
import           Data.Maybe       (fromJust)
import           GHC.Arr          (array)
import           GHC.Base         (undefined)
import           GHC.Float        (Floating (pi))
import           PlutusTx         as Tx
import           PlutusTx.Prelude hiding (fromInteger)
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

data Proof = Proof {
    piA :: [Integer]
  , piB :: [[Integer]]
  , piC :: [Integer]
  }

{-# INLINABLE arrayToG1 #-}
arrayToG1 :: [Integer] -> AffinePoint Fq1
arrayToG1 [x, y]    = Affine (fromInteger x) (fromInteger y)
arrayToG1 [x, y, _] = arrayToG1 [x, y]

{-# INLINABLE arrayToG2 #-}
arrayToG2 :: [[Integer]] -> JPoint Fq2
arrayToG2 [[x0, x1], [y0, y1], [z0, z1]] = JPoint (Fq2 (fromInteger x1) (fromInteger x0)) (Fq2 (fromInteger y1) (fromInteger y0)) (Fq2 (fromInteger z1) (fromInteger z0))


-- | Verify a proof given a verification key and a proof.
{-# INLINABLE verify #-}
verify :: VerificationKey -> Proof -> [Integer] -> Bool
verify vk pi pub =
  let
    n = nPublic vk

    eAB = pairing (arrayToG1 (piA pi)) (arrayToG2 (piB pi))
    eAlphaBeta = pairing (arrayToG1 (vkAlpha vk)) (arrayToG2 (vkBeta vk))

    vkICG1 = map (affineToJPoint . arrayToG1) (vkIC vk)
    vkI = foldl (+) (head vkICG1) $ zipWith jpointMul pub (tail vkICG1)
    eIGamma = pairing (jpointToAffine vkI) (arrayToG2 (vkGamma vk))

    eCDelta = pairing (arrayToG1 (piC pi)) (arrayToG2 (vkDelta vk))
  in
    eAB == eAlphaBeta * eIGamma
