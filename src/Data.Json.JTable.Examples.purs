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
import Text.Smolder.Markup ((!), text)


exampleRendererDefault = renderJTableDef

exampleRendererDebug = renderJTable defJTableOpts {style = debugStyle}

-- we use style instead of class to keep it self-contained
exampleRendererSemantic = renderJTable defJTableOpts {
    style = noStyle { td = \c j -> case toSemantic j of
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


exampleRendererAlphaColumnOrdering = 
  renderJTable defJTableOpts {columnOrdering = alphaOrdering}

exampleRendererAltHeader = 
  renderJTable defJTableOpts {insertHeaderCells = true}
