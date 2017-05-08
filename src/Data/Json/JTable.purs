module Data.Json.JTable
  ( renderJTable, renderJTableDef, renderJsonSimple
  , jTableOptsDefault
  , inOrdering, alphaOrdering
  , noStyle, bootstrapStyle, debugStyle
  , jtableComponent
  , module Reexports
  ) where

import Prelude

import Data.Argonaut.Core as J
import Data.Argonaut.JCursor (JsonPrim, runJsonPrim)
import Data.Array as A
import Data.Json.JTable.Internal (JTableQuery(..), JTableOpts) as Reexports
import Data.Json.JTable.Internal as JT
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

renderJsonSimple ∷ JsonPrim → String
renderJsonSimple j = runJsonPrim j (const "") show showNumber id
  where
  showNumber n =
    let s = show n
    in fromMaybe s (S.stripSuffix (S.Pattern ".0") s)

spans ∷ ∀ p r. Int → Int → Array (HP.IProp (colSpan ∷ Int, rowSpan ∷ Int | r) p)
spans w h =
  (if w > 1 then [ HP.colSpan w ] else [])
    <> (if h > 1 then [ HP.rowSpan h ] else [])

noStyle ∷ (JsonPrim → String) → JT.TableStyle
noStyle renderJson =
  { table: HH.table_
  , tr: HH.tr_
  , th: \l _ w h → HH.th (spans w h) [ HH.text l ]
  , td: \_ j w h → HH.td (spans w h) [ HH.text $ renderJson j ]
  }

bootstrapStyle ∷ (JsonPrim → String) → JT.TableStyle
bootstrapStyle renderJson = (noStyle renderJson)
  { table = HH.table [ HP.class_ (H.ClassName "table") ] }

debugStyle ∷ JT.TableStyle
debugStyle =
  { table: HH.table_
  , tr: HH.tr_
  , th: \_ p w h → HH.th (spans w h) [ HH.text $ S.joinWith "." $ A.fromFoldable p ]
  , td: \c j w h →
           HH.td
             (spans w h)
             [ HH.small
                 [ HP.class_ (H.ClassName "grey") ]
                 [ HH.text (show c) ]
             , HH.br_
             , HH.text $ show j
             ]
  }

inOrdering ∷ JT.ColumnOrdering
inOrdering _ _ _ _ = EQ

alphaOrdering ∷ JT.ColumnOrdering
alphaOrdering l1 _ l2 _ = compare l1 l2

jTableOptsDefault ∷ JT.JTableOpts
jTableOptsDefault =
  { style: noStyle renderJsonSimple
  , columnOrdering: inOrdering
  , insertHeaderCells: false
  , maxTupleSize: 10
  }

renderJTable ∷ ∀ f. JT.JTableOpts → J.Json → JT.Markup f
renderJTable = JT.renderJTableRaw

renderJTableDef ∷ ∀ f. J.Json → JT.Markup f
renderJTableDef = renderJTable jTableOptsDefault

jtableComponent ∷ ∀ g. JT.JTableOpts → H.Component HH.HTML JT.JTableQuery Unit Void g
jtableComponent opts =
  H.component
    { initialState: const J.jsonEmptyObject
    , render
    , eval
    , receiver: const Nothing
    }
  where
  render ∷ J.Json → H.ComponentHTML JT.JTableQuery
  render = renderJTable opts

  eval ∷ JT.JTableQuery ~> H.ComponentDSL J.Json JT.JTableQuery Void g
  eval (JT.SetJson json next) = H.put json $> next
