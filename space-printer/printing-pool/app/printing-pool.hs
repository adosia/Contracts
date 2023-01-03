import Prelude
import Cardano.Api
import PrintingPool ( printingPoolContractScript )

main :: IO ()
main = do
  result <- writeFileTextEnvelope "printing-pool.plutus" Nothing printingPoolContractScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
