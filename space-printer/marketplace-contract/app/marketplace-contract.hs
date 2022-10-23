import Prelude
import Cardano.Api
import MarketplaceContract ( lockingContractScript )

main :: IO ()
main = do
  result <- writeFileTextEnvelope "marketplace-contract.plutus" Nothing lockingContractScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
