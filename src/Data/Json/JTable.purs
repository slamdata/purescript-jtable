module Data.Json.JTable
  ( renderJTable, renderJTableDef
  , JTableOpts(..), jTableOptsDefault
  , ColumnOrdering(..), inOrdering, alphaOrdering
  , TableStyle(..), noStyle, bootstrapStyle, debugStyle
  ) where

import Data.Json.JTable.Internal

import Data.String (joinWith)
import qualified Data.Array.Unsafe as AU
import Data.Argonaut.Core
import Data.Argonaut.JCursor
import Text.Smolder.HTML (table, thead, tbody, tr, th, td, br, small)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup (Markup(..), MarkupM(..), Attributable, attribute, (!), text)
import Data.Foldable (mconcat)

-- type JPath = [String]

type TableStyle = {
  table :: Markup -> Markup,
  tr    :: Markup -> Markup ,
  th    :: JPath -> Markup,
  td    :: JCursor -> JsonPrim -> Markup }

renderJsonSimple j = runJsonPrim j (const "&nbsp;") show show id

noStyle = {
  table: table, 
  tr: tr, 
  th: \p -> th $ text $ AU.last p,
  td: \c j -> td $ text $ renderJsonSimple j
    
} :: TableStyle

bootstrapStyle = (noStyle {
  table = \m -> table ! attribute "class" "table" $ m}) :: TableStyle

debugStyle = (noStyle {
  th = (\p -> th $ text $ joinWith "." p),
  td = (\c j -> td $ mconcat $
    [(small ! className "grey" $ text $ show c), (br), (text $ show j)]
)}::TableStyle)


type ColumnOrdering = JPath -> JPath -> Ordering

inOrdering = (\p1 p2 -> EQ) :: ColumnOrdering
alphaOrdering = (\p1 p2 -> strcmp (AU.last p1) (AU.last p2)) :: ColumnOrdering


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
  th = (\t -> o.style.th (t # tPath)),
  td = (\c -> o.style.td (c # cCursor) (c # cJsonPrim)) }}

renderJTableDef :: Json -> Markup
renderJTableDef = renderJTable jTableOptsDefault
