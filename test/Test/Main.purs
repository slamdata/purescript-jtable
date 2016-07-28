module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log)

import Test.Data.Json.JSemantic as S
import Test.Data.Json.JTable as T
import Test.Data.Json.TestEffects (TestEffects)

main :: Eff (TestEffects ()) Unit
main = do
  log "Running JSemantic tests"
  S.main
  log "Running JTable tests"
  T.main
  log "Finished"
