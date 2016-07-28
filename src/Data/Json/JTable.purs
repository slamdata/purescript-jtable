module Data.Json.JTable
  ( renderJTable, renderJTableDef
  , jTableOptsDefault
  , inOrdering, alphaOrdering
  , noStyle, bootstrapStyle, debugStyle
  , jtableComponent
  , module Reexports
  ) where

import Prelude

import Data.Array as A
import Data.Argonaut.Core (Json)
import Data.Argonaut.JCursor (JsonPrim)
import Data.Json.JSemantic (toSemanticDef, renderJSemantic)
import Data.Json.JTable.Internal as JT
import Data.Json.JTable.Internal (JTableQuery(..), JTableOpts) as Reexports
import Data.String (joinWith)

import Halogen as H
import Halogen.Component as HC
import Halogen.HTML.Indexed as HI
import Halogen.HTML.Properties.Indexed as P

renderJsonSimple :: JsonPrim -> String
renderJsonSimple j = renderJSemantic $ toSemanticDef j

spans :: forall p r. Int -> Int -> Array (P.IProp (colSpan :: P.I, rowSpan :: P.I | r) p)
spans w h =
  (if w > 1 then [ P.colSpan w ] else [])
    <> (if h > 1 then [ P.rowSpan h ] else [])

noStyle :: JT.TableStyle
noStyle =
  { table : HI.table_
  , tr : HI.tr_
  , th : \l _ w h -> HI.th (spans w h) [ HI.text l ]
  , td : \_ j w h -> HI.td (spans w h) [ HI.text $ renderJsonSimple j ]
  }

bootstrapStyle :: JT.TableStyle
bootstrapStyle = noStyle { table = HI.table [ P.class_ (HI.className "table") ] }

debugStyle :: JT.TableStyle
debugStyle = noStyle
  { th = \_ p w h -> HI.th (spans w h) [ HI.text $ joinWith "." $ A.fromFoldable p ]
  , td = \c j w h ->
           HI.td
             (spans w h)
             [ HI.small
                 [ P.class_ (HI.className "grey") ]
                 [ HI.text (show c) ]
             , HI.br_
             , HI.text $ show j
             ]
  }

inOrdering :: JT.ColumnOrdering
inOrdering _ _ _ _ = EQ

alphaOrdering :: JT.ColumnOrdering
alphaOrdering l1 _ l2 _ = compare l1 l2

jTableOptsDefault :: JT.JTableOpts
jTableOptsDefault =
  { style: noStyle
  , columnOrdering: inOrdering
  , insertHeaderCells: false
  , maxTupleSize: 10
  }

renderJTable :: forall f. JT.JTableOpts -> Json -> JT.Markup f
renderJTable = JT.renderJTableRaw

renderJTableDef :: forall f. Json -> JT.Markup f
renderJTableDef = renderJTable jTableOptsDefault

jtableComponent :: forall g. JT.JTableOpts -> HC.Component Json JT.JTableQuery g
jtableComponent opts = HC.component { render, eval }
  where
  render :: Json -> HC.ComponentHTML JT.JTableQuery
  render = renderJTable opts

  eval :: JT.JTableQuery ~> HC.ComponentDSL Json JT.JTableQuery g
  eval (JT.SetJson json next) = H.modify (const json) $> next
