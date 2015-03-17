module Test.Main where

import Debug.Trace

import qualified Test.Data.Json.JTable.StrongCheck as SC
import qualified Test.Data.Json.JTable.Unit as U
import qualified Test.Data.Json.JSemantic as S

import Data.Either
import Test.Unit hiding (runTest)
import Control.Monad.Cont.Trans
import Control.Monad.Error.Trans
import Control.Monad.Eff
import Test.Unit.Console (TestOutput(..))

foreign import exit """function exit(rv){return function(){process.exit(rv);}}
""" :: forall e. Number -> Eff (testOutput :: TestOutput | e) Unit

runTest :: forall e. Test (testOutput :: TestOutput | e) -> Eff (testOutput :: TestOutput | e) Unit
runTest t = runContT (runErrorT t) \ei -> case ei of (Left _) -> exit 1
                                                     _ -> pure unit

main = do
  trace "Running JTable Unit Tests" 
  runTest U.main
  trace "Running JSemantic Unit Tests" 
  runTest S.main
  trace "Running JTable StrongCheck Tests" 
  SC.main
