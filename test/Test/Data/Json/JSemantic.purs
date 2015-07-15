module Test.Data.Json.JSemantic where

import Prelude
import Data.Maybe
import Data.Argonaut.Core
import Data.Argonaut.JCursor
import Data.Json.JSemantic

import qualified Data.Date as D
import Data.Argonaut.JCursor
import Test.StrongCheck
import Data.Maybe.Unsafe (fromJust)


assertion msg prim expected =
  assert ((toSemanticDef prim == expected) <?> msg)

d1 = "1981-04-01T06:55:00+02:00"
d2 = "2022-12-31T07:14:00+01:00"
i = d1 <> " - " <> d2

main = do
  assertion "null" primNull NA
  assertion "integral positive" (primNum 17.0) (Integral 17)
  assertion "integral negative" (primNum 0.0) (Integral 0)
  assertion "integral negative" (primNum (-3.0)) (Integral (-3))
  assertion "fractional positive" (primNum 1.5) (Fractional 1.5)
  assertion "fractional negative" (primNum (-91.349)) (Fractional (-91.349))
  assertion "true" (primBool true) (Bool true)
  assertion "false" (primBool false) (Bool false)
  assertion "datetime" (primStr d1) (DateTime $ fromJust $ D.fromString d1)
  assertion "interval" (primStr i) (Interval (fromJust $ D.fromString d1) (fromJust $ D.fromString d2))
  assertion "percent 0%" (primStr "0%") (Percent 0.0)
  assertion "percent 0.0%" (primStr "0.0%") (Percent 0.0)
  assertion "percent 13.91%" (primStr "13.91%") (Percent 13.91)
  assertion "percent -8.13%" (primStr "-8.13%") (Percent (-8.13))
  assertion "currency $0" (primStr "$0") (Currency 0.0)
  assertion "currency $1.23" (primStr "$1.23") (Currency 1.23)
  assertion "currency $123,456.03" (primStr "$123,456.03") (Currency 123456.03)
  assertion "currency $8,482,234" (primStr "$8,482,234") (Currency 8482234.0)
  assertion "text" (primStr "$0%") (Text "$0%")

