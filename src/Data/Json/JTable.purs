module Data.Json.JTable
  ( renderJTable, renderJTableDef
  , jTableOptsDefault
  , inOrdering, alphaOrdering
  , noStyle, bootstrapStyle, debugStyle
  ) where

import Data.String (joinWith)
import Data.Argonaut.Core (Json(..))
import Data.Argonaut.JCursor (JCursor(..), JsonPrim(..), runJsonPrim)
import Data.Foldable (mconcat)

import Data.Json.JTable.Internal (JPath(), Tree(..), Cell(..), Markup(), TableStyle(), ColumnOrdering(), JTableOpts(), renderJTableRaw, strcmp)

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A

renderJsonSimple :: JsonPrim -> String
renderJsonSimple j = runJsonPrim j (const "") show show id

noStyle :: TableStyle
noStyle = 
  { table: H.table_
  , tr:    H.tr_
  , th:    \l _ w h -> H.th [ A.colSpan w, A.rowSpan h ] [ H.text l ]
  , td:    \_ j w h -> H.td [ A.colSpan w, A.rowSpan h ] [ H.text $ renderJsonSimple j ]
  }

bootstrapStyle :: TableStyle
bootstrapStyle = noStyle { table = H.table [ A.class_ (A.className "table") ] }

debugStyle :: TableStyle
debugStyle = noStyle
  { th = \_ p w h -> H.th [ A.colSpan w, A.rowSpan h ] 
                          [ H.text $ joinWith "." p ]
  , td = \c j w h -> H.td [ A.colSpan w, A.rowSpan h ]  
                          [ H.small [ A.class_ (A.className "grey") ] [ H.text (show c) ]
                          , H.br_ []
                          , H.text (show j)
                          ]
  }

inOrdering :: ColumnOrdering
inOrdering _ _ _ _ = EQ

alphaOrdering :: ColumnOrdering
alphaOrdering l1 _ l2 _ = strcmp l1 l2

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
