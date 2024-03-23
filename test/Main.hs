module Main (main) where

import           Groth16     (verify)
import           Groth16Test (pi01, testProof01, testProof02, testProof03, vk01)
import           Test.Tasty  (defaultMain, testGroup)


main :: IO ()
main = defaultMain $ testGroup "Groth 16 verification tests" [testProof01, testProof02, testProof03]
