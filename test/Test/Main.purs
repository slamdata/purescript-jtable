module Test.Main where
import Prelude
import Control.Monad.Eff.Console
import qualified Test.Data.Json.JSemantic as S
import qualified Test.Data.Json.JTable as T
import Control.Monad.Eff


foreign import exit :: forall e. Eff e Unit
foreign import inPhantom :: forall e. Eff e Unit -> Eff e Unit

main :: Eff (T.TestEffects ()) Unit
main = inPhantom do
  log "Running JSemantic tests"
  S.main
  log "Running JTable tests"
  T.main
  log "Finished"
  exit
