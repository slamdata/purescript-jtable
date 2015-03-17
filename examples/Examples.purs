module Data.Json.JTable.Examples where

import Data.String (split)
import Data.Array (length, head, tail, filter, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst)
import Data.Either (Either(..))
import Data.Foreign (Foreign(..), F(..), readString, readNumber)
import Data.Foreign.Class (IsForeign)
import Data.Argonaut.Core (Json(..))
import Data.Argonaut.JCursor (primToJson, primNull)
import Data.Argonaut.Parser (jsonParser)
import Text.Smolder.HTML (tr, th, td, br, span, small)
import Text.Smolder.HTML.Attributes (style)
import Text.Smolder.Markup (Markup(..), (!), text)
import qualified Text.Smolder.Renderer.String as SmR
import Test.StrongCheck.Gen (GenState(..), runGen)
import Control.Monad.Eff (Eff(..))
import Control.Monad.Eff.Random (Random(..), random)
import Control.Monad.Trampoline (runTrampoline)
import Control.Bind ((<=<))
import Control.Timer (Timer(..))
import qualified Control.Monad.JQuery as J
import DOM (DOM(..))
import Debug.Spy (spy)

import Data.Json.JSemantic (JSemantic(..), toSemanticDef)
import Data.Json.Gen (genJson)
import Data.Json.JTable (renderJTable, renderJTableDef, jTableOptsDefault, 
                         debugStyle, noStyle, alphaOrdering)


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

exMaxTupleSIze = 
  renderJTable jTableOptsDefault {maxTupleSize = 3}

exRenderers = [ (Tuple "Default"        exDefault)
              , (Tuple "Debug"          exDebug)
              , (Tuple "Semantic"       exSemantic)
              , (Tuple "AlphaColumnOrd" exAlphaColumnOrd)
              , (Tuple "AltHeader"      exAltHeader)
              , (Tuple "MaxTupleSize"   exMaxTupleSIze )
              ]

getInput :: forall eff. Eff (dom::DOM | eff) (Either String (Tuple (Json -> Markup) Json))
getInput = do
  (Right s) <- (J.select "#in" >>= J.getValue) <#> readString
  (Right i) <- (J.select "#select_renderer" >>= J.getValue) <#> readString
  let j = jsonParser s
  return $ j >>= \json -> case head $ filter (fst >>> (== i)) exRenderers of 
    Just (Tuple _ renderer) -> Right $ Tuple renderer json
    _                       -> Left "Invalid Renderer"

render :: forall eff. Eff (dom::DOM | eff) Unit
render = do
  i <- getInput
  setHtml "#out" case i of Right (Tuple r json) -> SmR.render $ r json
                           Left e -> e

benchmark :: forall eff. Eff (dom::DOM, timer::Timer | eff) Unit
benchmark = 
  getInput >>= \i -> case i of 
    Left e -> setHtml "#out" e
    Right (Tuple r json) -> do
      ms <- _benchmark r json 
      setHtml "#benchmark" $ (show ms) ++ " ms"
      render

selectExample :: forall eff. Eff (dom::DOM | eff) Unit
selectExample = do
  (Right ex) <- (J.select "#select_example" >>= J.getValue) <#> readString
  jsonStr <- J.select ("#example-" ++ ex) >>= J.getText
  J.select "#in" >>= J.setValue jsonStr
  render

randomJson :: forall eff. Eff (random :: Random | eff) Json
randomJson = do
  i <- random
  let state = GenState {size: 10, seed: i}
  return $ fromMaybe (primToJson primNull) $
    (head <=< tail) $ fst $ runTrampoline $ runGen 2 state genJson

clickRandom :: forall eff. Eff (dom::DOM, random::Random | eff) Unit
clickRandom = do
  jsonStr <- randomJson
  J.select "#in" >>= J.setValue (show jsonStr)
  render


setHtml :: forall eff. String -> String -> Eff (dom::DOM | eff) Unit
setHtml i html = do 
  el <- J.select i
  J.clear el
  h <- J.create ("<span>" ++ html ++ "</span>")
  J.append h el
  return unit
  

main = J.ready do
  selectExample
  let on i ev cb = J.select i >>= J.on ev \ev _ -> cb
  on "#select_example"  "change" selectExample
  on "#select_renderer" "change" render
  on "#but_randomjson"  "click"  clickRandom
  on "#but_render"      "click"  render
  on "#but_benchmark"   "click"  benchmark
  onCtrlEnter "#in" render


foreign import onCtrlEnter """function onCtrlEnter (sel) { return function (cb) { return function () {
  window.jQuery(sel).on("keyup", function (e) {
    if ((e.keyCode == 10 || e.keyCode == 13) && e.ctrlKey) {cb()} }) }}}
""" :: forall eff. String -> Eff (dom :: DOM | eff) Unit -> Eff (dom :: DOM | eff) Unit

foreign import _benchmark """function _benchmark (renderer) { return function (json) { return function () {
  var t1 = performance.now()
  for (var i=0; i<100; i++) { var markup = renderer(json) }
  var t2 = performance.now()
  return Math.floor(t2 - t1) / 100 }}}
""" :: forall eff. (Json -> Markup) -> Json -> Eff (timer :: Timer | eff) Number
