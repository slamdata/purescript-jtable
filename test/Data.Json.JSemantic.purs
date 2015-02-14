module Test.Data.Json.JSemantic where

import Test.Unit
import Data.Maybe
import Data.Argonaut.Core
import Data.Argonaut.JCursor
import Text.Smolder.HTML (table, thead, tbody, tr, th, td, br, span, small)
import Text.Smolder.Markup ((!), text)
import qualified Text.Smolder.Renderer.String (render) as Sm
import qualified Data.Date as D
import Debug.Trace

import Data.Json.JSemantic


main = do
  let tf s j r = assert s $ toSemantic j == r
  let tfm s j r = case r of Just x -> assert s $ toSemantic j == x
                            Nothing -> assert s $ false
  test "toSemantic null" $ do
    tf "null" primNull NA
  test "toSemantic number" $ do
    tf "integral positive" (primNum 17) (Integral 17)
    tf "integral zero" (primNum 0) (Integral 0)
    tf "integral negative" (primNum (-3)) (Integral (-3))
    tf "fractional positive" (primNum 1.5) (Fractional (1.5))
    tf "fractional negative" (primNum (-91.349)) (Fractional (-91.349))
  test "toSemantic bool" $ do
    tf "true" (primBool true) (Bool true)
    tf "false" (primBool false) (Bool false)
  test "toSemantic date" $ do
    let d1 = "1981-04-01T06:55:00+02:00"
    let d2 = "2022-12-31T07:14:00+01:00"
    let i = d1 ++ " - " ++ d2
    tfm "datetime" (primStr d1) (DateTime <$> D.fromString d1)
    tfm "interval" (primStr i) (Interval <$> (D.fromString d1) <*> (D.fromString d2))
  test "toSemantic percent" $ do
    tf "percent 0%" (primStr "0%") (Percent 0)
    tf "percent 0.0%" (primStr "0.0%") (Percent 0)
    tf "percent 13.91%" (primStr "13.91%") (Percent 13.91)
    tf "percent -8.13%" (primStr "-8.23%") (Percent (-8.23))
  test "toSemantic currency" $ do
    tf "currency $0" (primStr "$0") (Currency 0)
    tf "currency $1.23" (primStr "$1.23") (Currency 1.23)
    tf "currency $123,456.03" (primStr "$123,456.03") (Currency 123456.03)
    tf "currency $8,482,234" (primStr "$8,482,234") (Currency 8482234)
  test "toSemantic text" $ do
    tf "text" (primStr "$0%") (Text "$0%")
