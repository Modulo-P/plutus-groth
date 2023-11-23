module Main where


import qualified Data.ByteString                          as BSL
import           Flat                                     (flat)
import           PlutusCore                               (DefaultFun,
                                                           DefaultUni,
                                                           NamedDeBruijn)
import qualified PlutusCore                               as PLC
import           UntypedPlutusCore                        (Program)
import qualified UntypedPlutusCore                        as UPLC
import qualified UntypedPlutusCore.Evaluation.Machine.Cek as Cek

import           BLS12_381_V2.Pairing_bls12381            as BLSV2
import PlutusLedgerApi.V2 ( ExCPU(..), ExBudget(..) )
import PlutusLedgerApi.V1 (ExMemory(..))
import qualified PlutusCore.Evaluation.Machine.ExBudgetingDefaults as PLC



main :: IO ()
main = do
  printProgramCosts BLSV2.testPlutusBLSV2
  printProgramCosts BLSV2.testMultiplyPoint


printProgramCosts prog = do
  let (cpu, mem) = getCostsCek prog
  putStrLn $ "CPU: " ++ "10000000000" ++ " MEM: " ++ "14000000"
  putStrLn $ "CPU: " ++ show cpu ++ " MEM: " ++ show mem

-- | Evaluate a script and return the CPU and memory costs (according to the cost model)

getCostsCek (UPLC.Program _ _ prog) =
    case Cek.runCekDeBruijn PLC.defaultCekParameters Cek.tallying Cek.noEmitter prog of
      (_res, Cek.TallyingSt _ budget, _logs) ->
          let ExBudget (ExCPU cpu)(ExMemory mem) = budget
          in (cpu, mem)

