module Main (main) where

import           Groth16     (verify)
import           Groth16Test (pi01, testProof01, vk01)
import           Test.Tasty  (defaultMain)


main :: IO ()
main = defaultMain testProof01
