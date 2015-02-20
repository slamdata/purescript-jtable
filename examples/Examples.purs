module Data.Json.JTable.Examples where

import Math
import Data.String (split)
import Data.Array (length)
import qualified Data.Array.Unsafe as AU
import Data.Json.JTable
import Data.Json.JSemantic
import Data.Argonaut.Core
import Text.Smolder.HTML (tr, th, td, br, span, small)
import Text.Smolder.HTML.Attributes (style)
import Text.Smolder.Markup (Markup(..), (!), text)
import Text.Smolder.Renderer.String (render)
import Control.Monad.Eff


exDefault = renderJTableDef

exDebug = renderJTable jTableOptsDefault {style = debugStyle}

-- we use style instead of class to keep it self-contained
exSemantic = renderJTable jTableOptsDefault {
    style = noStyle { td = \c j -> case toSemanticDef j of
      Integral   n -> td ! style "text-align:right" $ text $ show n
      Fractional n -> td ! style "text-align:right" $ do
        let s = split "." (show n)
        text $ AU.head $ s
        if length s > 1 
          then small ! style "color:#aaa" $ text $  "." ++ (AU.head $ AU.tail s)
          else text ""
      Date       d -> td $ text $ show d
      DateTime   d -> td $ text $ show d
      Time       d -> td $ text $ show d
      Interval u v -> td $ text $ show u ++ " - " ++ show v
      Text       s -> td $ text $ "&laquo;" ++ s ++ "&raquo;"
      Bool       b -> 
        if b then td ! style "background:#cfc; text-align:center" $ text "✔"
             else td ! style "background:#fcc; text-align:center" $ text "❌"
      Percent    n -> td ! style "text-align: right" $ case compare n 0 of
        LT -> span ! style "color:#a00" $ text $ show (0-n) ++ "% ▾"
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


foreign import _main """
function _main (render_markup) { return function (examples) {
  window.addEventListener("load", function () {
    var S = function (id) { return document.getElementById(id) }
    
    var render = function () { try { 
      var inp = JSON.parse(S("in").value)
      var renderer = examples[S("select_renderer").value]
      var out = render_markup(renderer(inp))
      S("out").innerHTML = out
    } catch (err) { S("out").innerText = err; throw err}}
    
    var benchmark = function () { try {
      var inp = JSON.parse(S("in").value)
      var renderer = examples[S("select_renderer").value]
      var markup;
      var t1 = performance.now()
      for (var i=0; i<100; i++) {
        markup = renderer(inp) }
      var t2 = performance.now()
      var out = render_markup(markup)
      S("out").innerHTML = out
      S("benchmark").innerText = (Math.floor(t2 - t1) / 100) + " ms"
    } catch (err) { S("out").innerText = err; throw err}}

    var select_example = function () {
      S("in").value = S("example-" + S("select").value).innerHTML 
      render() }
    
    S("select").addEventListener("change", select_example) 
    S("select_renderer").addEventListener("change", render) 
    S("but_render").addEventListener("click", render)
    S("but_benchmark").addEventListener("click", benchmark)
    S("in").addEventListener("keydown", function(e) {
      if ((e.keyCode == 10 || e.keyCode == 13) && e.ctrlKey) { render() } } )
    
    select_example() 
  })
}}""" :: forall eff. (Markup -> String) -> [Json -> Markup] -> Eff eff Unit


import Debug.Trace
main = do
  _main render [ exDefault
               , exDebug
               , exSemantic
               , exAlphaColumnOrd
               , exAltHeader
               , exHomoTupSize 
               ]
