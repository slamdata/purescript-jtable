module Data.Json.JTable.Examples where

import Math
import Data.String (split)
import Data.Array (length, head, tail, (!!))
import Data.Maybe
import Data.Tuple
import Data.Either
import Data.Argonaut.Core
import Data.Argonaut.JCursor (primToJson, primNull)
import Data.Argonaut.Parser
import Text.Smolder.HTML (tr, th, td, br, span, small)
import Text.Smolder.HTML.Attributes (style)
import Text.Smolder.Markup (Markup(..), (!), text)
import qualified Text.Smolder.Renderer.String as SmR
import Test.StrongCheck
import Test.StrongCheck.Gen
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Trampoline
import Control.Bind
import Control.Timer
import DOM
import Debug.Trace
import Debug.Spy

import Data.Json.JTable
import Data.Json.JSemantic
import Data.Json.Gen


exDefault = renderJTableDef

exDebug = renderJTable jTableOptsDefault {style = debugStyle}

-- we use style instead of class to keep it self-contained
exSemantic = renderJTable jTableOptsDefault {
    style = noStyle { td = \c j -> case toSemanticDef j of
      Integral   n -> td ! style "text-align:right" $ text $ show n
      Fractional n -> td ! style "text-align:right" $ 
        case split "." (show n) of
          (s1 : s2 : []) -> do
            text $ s1
            small ! style "color:#aaa" $ text $  "." ++ s2
          _ -> text $ (show n)
      Date       d -> td $ text $ show d
      DateTime   d -> td $ text $ show d
      Time       d -> td $ text $ show d
      Interval u v -> td $ text $ show u ++ " - " ++ show v
      Text       s -> td $ text $ "&laquo;" ++ s ++ "&raquo;"
      Bool       b -> 
        if b then td ! style "background:#cfc; text-align:center" $ text "✔"
             else td ! style "background:#fcc; text-align:center" $ text "❌"
      Percent    n -> td ! style "text-align: right" $ case compare n 0 of
        LT -> span ! style "color:#a00" $ text $ show n ++ "% ▾"
        EQ -> span ! style "color:#000" $ text $ show n ++ "% ◂"
        GT -> span ! style "color:#0a0" $ text $ show n ++ "% ▴"
      Currency   n -> td $ text $ show n
      NA           -> td ! style "background:#ddd" $ text "&nbsp;"
  }}


exAlphaColumnOrd = 
  renderJTable jTableOptsDefault {columnOrdering = alphaOrdering}

exAltHeader = 
  renderJTable jTableOptsDefault {insertHeaderCells = true}

exHomoTupSize = 
  renderJTable jTableOptsDefault {maxHomoTupSize = 5}

exRenderers = [ exDefault
              , exDebug
              , exSemantic
              , exAlphaColumnOrd
              , exAltHeader
              , exHomoTupSize 
              ]

getInput = do
  s <- getValue "in"
  let j = jsonParser s
  i <- getNumberValue "select_renderer"
  return $ j >>= \json -> case exRenderers !! i of 
    Just renderer -> Right $ Tuple renderer json
    _      -> Left "Invalid Renderer"

render = 
  getInput >>= \i -> setHtml "out" case i of 
    Left e -> e
    Right (Tuple renderer json) -> SmR.render $ renderer json

benchmark = 
  getInput >>= \i -> case i of 
    Left e -> setHtml "out" e
    Right (Tuple r json) -> do
      ms <- _benchmark r json 
      setHtml "benchmark" $ (show ms) ++ " ms"
      render

select_example = do
  ex <- getValue "select_example"
  jsonStr <- getHtml ("example-" ++ ex)
  setValue "in" jsonStr
  render

click_random = do
  jsonStr <- randomJson
  setValue "in" $ show jsonStr
  render

randomJson = do
  i <- random
  let state = GenState {size: 10, seed: i}
  return $ fromMaybe (primToJson primNull) $
    (head <=< tail) $ fst $ runTrampoline $ runGen 2 state genJson

main = do
  select_example
  onload do
    on "change" "select_example" select_example
    on "change" "select_renderer" render
    on "click" "but_randomjson"  click_random
    on "click" "but_render" render
    on "click" "but_benchmark"  benchmark
    onCtrlEnter "in" render


-- simple dom access

foreign import getValue """function getValue (id) { return function () {
  return document.getElementById(id).value }}
""" :: forall eff. String -> Eff (dom :: DOM | eff) String

foreign import getNumberValue """function getNumberValue (id) { return function () {
  return document.getElementById(id).value * 1 }}
""" :: forall eff. String -> Eff (dom :: DOM | eff) Number

foreign import setValue """function setValue (id) { return function (val) { return function () {
  return document.getElementById(id).value = val }}}
""" :: forall eff. String -> String -> Eff (dom :: DOM | eff) String

foreign import getHtml """function getHtml (id) { return function () {
  return document.getElementById(id).innerHTML }}
""" :: forall eff. String -> Eff (dom :: DOM | eff) String

foreign import setHtml """function setHtml (id) { return function (html) { return function () {
  document.getElementById(id).innerHTML = html }}}
""" :: forall eff. String -> String -> Eff (dom :: DOM | eff) Unit

foreign import on """function on (ev) { return function (id) { return function (cb) { return function () {
  document.getElementById(id).addEventListener(ev, cb) }}}}
""" :: forall eff. String -> String -> Eff (dom :: DOM | eff) Unit -> Eff (dom :: DOM | eff) Unit

foreign import onload """function onload (cb) { return function () {
  window.addEventListener("load", cb) }}
""" :: forall eff. Eff (dom :: DOM | eff) Unit -> Eff (dom :: DOM | eff) Unit

foreign import onCtrlEnter """function onCtrlEnter (id) { return function (cb) { return function () {
  on("keydown")(id)(function (e) {
    if ((e.keyCode == 10 || e.keyCode == 13) && e.ctrlKey) {cb()} }) }}}
""" :: forall eff. String -> Eff (dom :: DOM | eff) Unit -> Eff (dom :: DOM | eff) Unit

foreign import _benchmark """function _benchmark (renderer) { return function (json) { return function () {
  var t1 = performance.now()
  for (var i=0; i<100; i++) { var markup = renderer(json) }
  var t2 = performance.now()
  return Math.floor(t2 - t1) / 100 }}}
""" :: forall eff. (Json -> Markup) -> Json -> Eff (timer :: Timer | eff) Number
