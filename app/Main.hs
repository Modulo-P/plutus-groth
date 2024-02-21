module Main where


import qualified Data.ByteString                                   as BSL
import           Flat                                              (flat)
import           PlutusCore                                        (DefaultFun,
                                                                    DefaultUni,
                                                                    NamedDeBruijn)
import qualified PlutusCore                                        as PLC
import qualified UntypedPlutusCore                                 as UPLC
import           UntypedPlutusCore                                 (Program)
import qualified UntypedPlutusCore.Evaluation.Machine.Cek          as Cek

import           BLS12381                                          as BLS
import           Groth16Plc                                        (getPlcProof01)
import qualified PlutusCore.Evaluation.Machine.ExBudgetingDefaults as PLC
import           PlutusLedgerApi.V1                                (ExMemory (..))
import           PlutusLedgerApi.V2                                (ExBudget (..),
                                                                    ExCPU (..))



main :: IO ()
main = do
  putStrLn "Plutus Core Groth16 benchmarks"
  putStrLn "==============================="
  putStrLn ""
  putStrLn "Single pairing benchmark"
  printProgramCosts BLS.testPlutusBLSV2
  putStrLn ""
  putStrLn "Point multiplication benchmark"
  printProgramCosts BLS.testMultiplyPoint
  putStrLn ""
  putStrLn "Complete proof benchmark"
  printProgramCosts getPlcProof01


printProgramCosts prog = do
  let (cpu, mem) = getCostsCek prog
  putStrLn $ "CPU: " ++ "10000000000" ++ " MEM: " ++ "14000000 (Maximum budget)"
  putStrLn $ "CPU: " ++ show cpu ++ " MEM: " ++ show mem ++ " (Actual costs)"

-- | Evaluate a script and return the CPU and memory costs (according to the cost model)

getCostsCek (UPLC.Program _ _ prog) =
    case Cek.runCekDeBruijn PLC.defaultCekParameters Cek.tallying Cek.noEmitter prog of
      (_res, Cek.TallyingSt _ budget, _logs) ->
          let ExBudget (ExCPU cpu)(ExMemory mem) = budget
          in (cpu, mem)

