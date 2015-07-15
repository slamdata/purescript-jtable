module Data.Json.JTable 
  ( renderJTable, renderJTableDef
  , jTableOptsDefault
  , inOrdering, alphaOrdering
  , noStyle, bootstrapStyle, debugStyle
  ) where

import Prelude
import Data.String (joinWith)
import Data.Argonaut.Core (Json(..))
import Data.Argonaut.JCursor (JCursor(..), JsonPrim(..), runJsonPrim)
import Data.Json.JSemantic (toSemanticDef, JSemantic(..), renderJSemantic)
import Data.List (fromList)

import Data.Json.JTable.Internal 

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A

-- TODO: colSpan/rowSpan from number to int
rowSpan :: forall i. Int -> A.Attr i
rowSpan = A.attr (A.attributeName "rowSpan") <<< show

colSpan :: forall i. Int -> A.Attr i
colSpan = A.attr (A.attributeName "colSpan") <<< show

renderJsonSimple :: JsonPrim -> String
renderJsonSimple j = renderJSemantic $  toSemanticDef j 


spans w h = 
  (if w > 1 then [ colSpan w] else [ ]) <>
  (if h > 1 then [ rowSpan h] else [ ]) <>
  [ ]
                                               
noStyle :: TableStyle
noStyle = 
  { table: H.table_
  , tr:    H.tr_
  , th:    \l _ w h -> H.th (spans w h)
                       [ H.text l ]
  , td:    \_ j w h -> H.td (spans w h) 
                       [ H.text $ renderJsonSimple j ]
  }

bootstrapStyle :: TableStyle
bootstrapStyle = noStyle { table = H.table [ A.class_ (A.className "table") ] }

debugStyle :: TableStyle
debugStyle = noStyle
  { th = \_ p w h -> H.th (spans w h)
                          [ H.text $ joinWith "." $ fromList p ]
  , td = \c j w h -> H.td (spans w h)
                          [ H.small [ A.class_ (A.className "grey") ]
                            [ H.text (show c) ]
                          , H.br_ []
                          , H.text (show j)
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
  
renderJTable :: JTableOpts -> Json -> Markup
renderJTable = renderJTableRaw

renderJTableDef :: Json -> Markup
renderJTableDef = renderJTable jTableOptsDefault
