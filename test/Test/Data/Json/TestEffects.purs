module Test.Data.Json.TestEffects where

import Prelude

import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Random (RANDOM())

type TestEffects eff =
  ( err :: EXCEPTION
  , random :: RANDOM
  , console :: CONSOLE
  | eff
  )
