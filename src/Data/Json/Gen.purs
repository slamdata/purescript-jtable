module Data.Json.Gen where

import qualified Data.Argonaut as A
import Data.Argonaut.Core (Json(..), jsonNull)
import qualified Data.StrMap as SM

import Test.StrongCheck
import Test.StrongCheck.Gen

genJsonArray :: Gen Json
genJsonArray = sized \size -> 
  (vectorOf size $ resize (size-1) genJson) <#> A.fromArray

genJsonObject :: Gen Json
genJsonObject = sized \size -> 
  (vectorOf size $ resize (size-1) arbitrary) <#> SM.fromList >>> A.fromObject

genJson :: Gen Json
genJson = sized \size -> 
  if size == 0 
  then oneOf (pure jsonNull) [
    arbitrary <#> A.fromBoolean,
    arbitrary <#> A.fromNumber,
    arbitrary <#> A.fromBoolean ] 
  else oneOf (pure jsonNull) [
    resize (size-1) genJsonArray,
    resize (size-1) genJsonObject ]

instance arbJson :: Arbitrary Json where
  arbitrary = genJson
