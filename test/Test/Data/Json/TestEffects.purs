module Test.Data.Json.TestEffects where

import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)

type TestEffects eff =
  ( exception ∷ EXCEPTION
  , random ∷ RANDOM
  , console ∷ CONSOLE
  | eff
  )
