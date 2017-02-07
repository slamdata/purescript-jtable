module Main where

import Prelude

import Control.Monad.Eff (Eff)

import Data.Argonaut.Core (jsonEmptyArray)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Json.JTable as J
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

type DemoState = String
data DemoQuery a = SetJsonText String a
type DemoSlot = Unit

ui ∷ ∀ m. J.JTableOpts → H.Component HH.HTML DemoQuery Unit Void m
ui opts =
  H.parentComponent
    { initialState: const ""
    , render
    , eval
    , receiver: const Nothing
    }
  where
  render ∷ DemoState → H.ParentHTML DemoQuery J.JTableQuery DemoSlot m
  render jsonString =
    HH.div
      [ HP.class_ $ HH.ClassName "container" ]
      [ HH.h1_ [ HH.text "purescript-jtable demo" ]
      , HH.p_ [ HH.text "Paste some JSON:" ]
      , HH.p_
          [ HH.textarea
              [ HP.class_ $ HH.ClassName "form-control"
              , HP.value jsonString
              , HE.onValueInput $ HE.input SetJsonText
              ]
          ]
      , HH.h2_ [ HH.text "Output" ]
      , HH.slot unit (J.jtableComponent opts) unit absurd
      ]

  eval ∷ DemoQuery ~> H.ParentDSL DemoState DemoQuery J.JTableQuery DemoSlot Void m
  eval (SetJsonText jsonString next) = do
    H.query unit <<< H.action <<< J.SetJson $
      case jsonParser jsonString of
        Left _ → jsonEmptyArray
        Right json → json
    pure next

main ∷ Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff $ runUI (ui J.jTableOptsDefault) unit =<< HA.awaitBody
