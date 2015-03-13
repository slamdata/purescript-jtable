module Test.Data.Json.JTable.Unit where

import Test.Unit (test, assert)
import Data.Maybe (Maybe(..))
import Data.Argonaut.Core (Json(..))
import Data.Argonaut.JCursor (primToJson, primNull)
import Text.Smolder.HTML (table, thead, tbody, tr, th, td, br, span, small)
import Text.Smolder.Markup ((!), text)
import qualified Text.Smolder.Renderer.String (render) as Sm
import Debug.Spy (spy)
-- import Data.Traversable (for)

import Data.Json.JTable.Internal (widthOfPrimTuple, strcmp, renderJTableRaw)
import Data.Json.JTable (renderJTable, renderJTableDef, jTableOptsDefault)


jNull = primToJson primNull
foreign import j0 "var j0 = 0" :: Json
foreign import jTup2  "var jTup2 = ['hi', 1]" :: Json
foreign import jATup2 "var jATup2 = jTup2" :: [Json]
foreign import jTup3  "var jTup3 = [null, 'one', 2]" :: Json
foreign import jATup3 "var jATup3 = jTup3" :: [Json]
foreign import jHomoTup2  "var jHomoTup2 = [1,2]" :: Json
foreign import jAHomoTup2 "var jAHomoTup2 = jHomoTup2" :: [Json]
foreign import jArr3  "var jArr3 = [true, false, true]" :: Json
foreign import jAArr3 "var jAArr3 = jArr3" :: [Json]
foreign import jObj2 "var jObj2 = {a:1, b:'two'}" :: Json
foreign import jObjArr0 "var jObjArr0 = {a:[]}" :: Json
foreign import jObj2Tup2 "var jObj2Tup2 = {a:1, b:['one', false]}" :: Json
foreign import jObj2Obj2 "var jObj2Obj2 = {a:1, b: {b1: 'one', b2: false}}" :: Json
foreign import jObjArr2Tup2 "var jObjArr2Tup2 = {a:[[1, 'two'], [3, 'four']]}" :: Json
foreign import jTup2Obj "var jTup2Obj = [{x:1}, {y:2}]" :: [Json]
foreign import jObjTup2Obj "var jObjTup2Obj = {a:[{x:1}, {y:2}]}" :: Json
foreign import jObj2EArr "var jObj2EArr = {a:[], b:'one'}" :: Json
foreign import jObjWeird "var jObjWeird = {a:{x:[1,2,3,4], y:[]}, b:[1,2,3,4,5]}" :: Json
foreign import jArrObj2Tups "var jArrObj2Tups = [{a:[3,2,1],b:[1,2]},{a:null,b:[3,2,1]}]" :: Json
foreign import jMergeObjTup "var jMergeObjTup = [{a:{x:0}, b:8}, {a: [1,2], b:9}]" :: Json


main = do
  test "widthOfPrimTuple" do
    let tf s t j r = assert s $ (widthOfPrimTuple 2 t j) == r
    tf "top null" [] [jNull] Nothing
    tf "top jATup2" [] jATup2 Nothing
    tf "top jATup3" [] jATup3 Nothing
    tf "null" [""] [jNull] Nothing
    tf "0" [""] [j0] Nothing
    tf "jATup2" [""] jATup2 $ Just 2
    tf "jATup3" [""] jATup3 $ Nothing
    tf "jAHomoTup2" [""] jAHomoTup2 $ Just 2
    tf "jAArr3" [""] jAArr3 Nothing
    tf "tup 0 arr" [""] [j0, jTup2] Nothing
    tf "tup 0 obj" [""] [j0, jObj2] Nothing
    tf "tup 0 obj" [""] [j0, jObj2] Nothing
    tf "jTup2Obj" [""] jTup2Obj Nothing
  test "strcmp" do
    let tf a b r = assert ("strcmp '" ++ a ++ "' '" ++ b ++ "'") $ strcmp a b == r
    tf "" "" EQ
    tf "a" "a" EQ
    tf "" "a" LT
    tf "a" "" GT
    tf "a" "b" LT
    tf "algebraic" "propositional" LT
    tf "algebraic" "Propositional" GT
    tf "monadic" "diadic" GT
    tf "és" "平仮名" LT
    tf "e" "é" LT
    tf "É" "e" GT
  test "renderJTableRaw" do
    let tf s j r = assert s $ (Sm.render $ renderJTableDef j) == r
    tf "null" jNull $ 
      "<table><thead/><tbody><tr><td>&nbsp;</td></tr></tbody></table>"
    tf "0" j0 $ 
      "<table><thead/><tbody><tr><td>0</td></tr></tbody></table>"
    tf "jObj2" jObj2 $ 
      "<table><thead><tr><th>a</th><th>b</th></tr></thead>" ++ 
      "<tbody><tr><td>1</td><td>two</td></tr></tbody></table>"
    tf "jObj2Tup2" jObj2Tup2 $ 
      "<table><thead><tr><th>a</th><th colspan=\"2\">b</th></tr></thead>" ++ 
      "<tbody><tr><td>1</td><td>one</td><td>false</td></tr></tbody></table>"
    tf "jObj2Obj2" jObj2Obj2 $ 
      "<table><thead><tr><th rowspan=\"2\">a</th><th colspan=\"2\">b</th></tr>" ++ 
      "<tr><th>b1</th><th>b2</th></tr></thead>" ++ 
      "<tbody><tr><td>1</td><td>one</td><td>false</td></tr></tbody></table>"
    tf "jObjArr2Tup2" jObjArr2Tup2 $ 
      "<table><thead><tr><th colspan=\"2\">a</th></tr></thead>" ++ 
      "<tbody><tr><td>1</td><td>two</td></tr><tr><td>3</td><td>four</td></tr></tbody></table>"
    tf "jObjTup2Obj" jObjTup2Obj $ 
      "<table><thead><tr><th colspan=\"2\">a</th></tr>" ++ 
      "<tr><th>x</th><th>y</th></tr></thead>" ++ 
      "<tbody><tr><td>1</td><td>2</td></tr></tbody></table>"
    tf "jObj2EArr" jObj2EArr $ 
      "<table><thead><tr><th>a</th><th>b</th></tr></thead>" ++ 
      "<tbody><tr><td>&nbsp;</td><td>one</td></tr></tbody></table>"
    tf "jObjWeird" jObjWeird $ 
      "<table><thead><tr><th colspan=\"5\">a</th><th colspan=\"5\" rowspan=\"2\">b</th></tr><tr><th colspan=\"4\">x</th><th>y</th></tr></thead><tbody><tr><td>1</td><td>2</td><td>3</td><td>4</td><td>&nbsp;</td><td>1</td><td>2</td><td>3</td><td>4</td><td>5</td></tr></tbody></table>"
    tf "jArrObj2Tups" jArrObj2Tups $ 
      "<table><thead><tr><th colspan=\"3\">a</th><th colspan=\"3\">b</th></tr></thead><tbody><tr><td>3</td><td>2</td><td>1</td><td>1</td><td>2</td><td>&nbsp;</td></tr><tr><td colspan=\"3\">&nbsp;</td><td>3</td><td>2</td><td>1</td></tr></tbody></table>"
    tf "jObjArr0" jObjArr0 $ 
      "<table><thead><tr><th>a</th></tr></thead><tbody><tr><td>&nbsp;</td></tr></tbody></table>"
    tf "jMergeObjTup" jMergeObjTup $ 
      "<table><thead><tr><th>a</th><th rowspan=\"2\">b</th></tr><tr><th>x</th></tr></thead><tbody><tr><td>0</td><td>8</td></tr><tr><td>1</td><td rowspan=\"2\">9</td></tr><tr><td>2</td></tr></tbody></table>"

  test "insertHeaderCells" do
    let o = jTableOptsDefault {insertHeaderCells = true}
    let tf s j r = assert s $ (Sm.render $ renderJTable o j) == r
    tf "jObj2Obj2" jObj2Obj2 $ 
      "<table><thead><tr><th></th><th colspan=\"2\">b</th></tr>" ++ 
      "<tr><th>a</th><th>b1</th><th>b2</th></tr></thead>" ++ 
      "<tbody><tr><td>1</td><td>one</td><td>false</td></tr></tbody></table>"
