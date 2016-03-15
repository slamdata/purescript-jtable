module Data.Json.JTable
  ( renderJTable, renderJTableDef
  , jTableOptsDefault
  , inOrdering, alphaOrdering
  , noStyle, bootstrapStyle, debugStyle
  , jtableComponent
  , module I
  ) where

import Prelude

import Data.Argonaut.Core (Json())
import Data.Argonaut.JCursor (JsonPrim())
import Data.Functor (($>))
import Data.Json.JSemantic (toSemanticDef, renderJSemantic)
import Data.Json.JTable.Internal
import Data.Json.JTable.Internal (JTableQuery(..), JTableOpts()) as I
import Data.List (fromList)
import Data.NaturalTransformation (Natural())
import Data.String (joinWith)

import Halogen (modify) as H
import Halogen.Component as H
import Halogen.HTML.Indexed as H
import Halogen.HTML.Properties.Indexed as P

renderJsonSimple :: JsonPrim -> String
renderJsonSimple j = renderJSemantic $ toSemanticDef j

spans :: forall p r. Int -> Int -> Array (P.IProp (colSpan :: P.I, rowSpan :: P.I | r) p)
spans w h =
  (if w > 1 then [ P.colSpan w ] else [])
    <> (if h > 1 then [ P.rowSpan h ] else [])

noStyle :: TableStyle
noStyle =
  { table : H.table_
  , tr : H.tr_
  , th : \l _ w h -> H.th (spans w h) [ H.text l ]
  , td : \_ j w h -> H.td (spans w h) [ H.text $ renderJsonSimple j ]
  }

bootstrapStyle :: TableStyle
bootstrapStyle = noStyle { table = H.table [ P.class_ (H.className "table") ] }

debugStyle :: TableStyle
debugStyle = noStyle
  { th = \_ p w h -> H.th (spans w h) [ H.text $ joinWith "." $ fromList p ]
  , td = \c j w h ->
           H.td
             (spans w h)
             [ H.small
                 [ P.class_ (H.className "grey") ]
                 [ H.text (show c) ]
             , H.br_
             , H.text $ show j
             ]
  }

inOrdering :: ColumnOrdering
inOrdering _ _ _ _ = EQ

alphaOrdering :: ColumnOrdering
alphaOrdering l1 _ l2 _ = compare l1 l2

jTableOptsDefault :: JTableOpts
jTableOptsDefault =
  { style: noStyle
  , columnOrdering: inOrdering
  , insertHeaderCells: false
  , maxTupleSize: 10
  }

renderJTable :: forall f. JTableOpts -> Json -> Markup f
renderJTable = renderJTableRaw

renderJTableDef :: forall f. Json -> Markup f
renderJTableDef = renderJTable jTableOptsDefault

jtableComponent :: forall g. JTableOpts -> H.Component Json JTableQuery g
jtableComponent opts = H.component { render, eval }
  where
  render :: Json -> H.ComponentHTML JTableQuery
  render = renderJTable opts

  eval :: Natural JTableQuery (H.ComponentDSL Json JTableQuery g)
  eval (SetJson json next) = H.modify (const json) $> next
