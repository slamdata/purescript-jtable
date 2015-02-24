module Data.Json.JTable
  ( renderJTable, renderJTableDef
  , JTableOpts(..), jTableOptsDefault
  , ColumnOrdering(..), inOrdering, alphaOrdering
  , TableStyle(..), noStyle, bootstrapStyle, debugStyle
  ) where

import Data.String (joinWith)
import Data.Argonaut.Core (Json(..))
import Data.Argonaut.JCursor (JCursor(..), JsonPrim(..), runJsonPrim)
import Text.Smolder.HTML (table, thead, tbody, tr, th, td, br, small)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (Markup(..), MarkupM(..), Attributable, attribute, (!), text)
import Data.Foldable (mconcat)

import Data.Json.JTable.Internal (JPath(..), Tree(..), Cell(..), renderJTableRaw, strcmp)

-- type JPath = [String]

type TableStyle = {
  table :: Markup -> Markup,
  tr    :: Markup -> Markup ,
  th    :: String -> JPath -> Markup,
  td    :: JCursor -> JsonPrim -> Markup }

renderJsonSimple j = runJsonPrim j (const "&nbsp;") show show id

noStyle = {
  table: table, 
  tr: tr, 
  th: \l p -> th $ text l,
  td: \c j -> td $ text $ renderJsonSimple j
    
} :: TableStyle

bootstrapStyle = (noStyle {
  table = \m -> table ! attribute "class" "table" $ m}) :: TableStyle

debugStyle = (noStyle {
  th = (\l p -> th $ text $ joinWith "." p),
  td = (\c j -> td $ mconcat $
    [(small ! className "grey" $ text $ show c), (br), (text $ show j)]
)}::TableStyle)


type ColumnOrdering = String -> JPath -> String -> JPath -> Ordering

inOrdering = (\l1 p1 l2 p2 -> EQ) :: ColumnOrdering
alphaOrdering = (\l1 p1 l2 p2 -> strcmp l1 l2) :: ColumnOrdering


type JTableOpts = {
  style :: TableStyle,
  columnOrdering :: ColumnOrdering,
  insertHeaderCells :: Boolean,
  maxHomoTupSize :: Number }

jTableOptsDefault  = {
  style: noStyle,
  columnOrdering: inOrdering,
  insertHeaderCells: false,
  maxHomoTupSize: 3
} :: JTableOpts


renderJTable :: JTableOpts -> Json -> Markup
renderJTable o = renderJTableRaw o { style = o.style {
  th = (\(T l p _ _ _) -> o.style.th l p),
  td = (\(C c _ _ j) -> o.style.td c j) }}

renderJTableDef :: Json -> Markup
renderJTableDef = renderJTable jTableOptsDefault
