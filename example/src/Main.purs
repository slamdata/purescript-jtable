module Main where

import Prelude

import Control.Monad.Aff.AVar as Aff
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)

import Data.Argonaut.Core (Json, jsonEmptyArray)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Functor.Coproduct (Coproduct)
import Data.Json.JTable as J
import Data.Maybe (Maybe(..))

import DOM (DOM)

import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as HP
import Halogen.Util (runHalogenAff, awaitBody)

type DemoState = String
data DemoQuery a = SetJsonText String a

type DemoSlot = Unit
type DemoInstalledState g = H.ParentState DemoState Json DemoQuery J.JTableQuery g DemoSlot
type DemoComponent g = H.Component (DemoInstalledState g) (Coproduct DemoQuery (H.ChildF DemoSlot J.JTableQuery)) g
type DemoRender g = DemoState -> H.ParentHTML Json DemoQuery J.JTableQuery g DemoSlot
type DemoEval g = DemoQuery ~> H.ParentDSL DemoState Json DemoQuery J.JTableQuery g DemoSlot

ui :: forall g. (Functor g) => J.JTableOpts -> DemoComponent g
ui opts = H.parentComponent { render, eval, peek: Nothing }
  where
  render :: DemoRender g
  render jsonString =
    HH.div
      [ HP.class_ $ HH.className "container" ]
      [ HH.h1_ [ HH.text "purescript-jtable demo" ]
      , HH.p_ [ HH.text "Paste some JSON:" ]
      , HH.p_
          [ HH.textarea
              [ HP.class_ $ HH.className "form-control"
              , HP.value jsonString
              , HE.onValueInput $ HE.input SetJsonText
              ]
          ]
      , HH.h2_ [ HH.text "Output" ]
      , HH.slot unit \_ ->
          { component : J.jtableComponent opts
          , initialState : jsonEmptyArray
          }
      ]

  eval :: DemoEval g
  eval (SetJsonText jsonString next) = do
    H.query unit <<< H.action <<< J.SetJson $
      case jsonParser jsonString of
        Left _ -> jsonEmptyArray
        Right json -> json
    pure next

type DemoEffects =
  ( dom :: DOM
  , avar :: Aff.AVAR
  , err :: EXCEPTION
  )

main :: Eff DemoEffects Unit
main = runHalogenAff $
  H.runUI (ui J.jTableOptsDefault) (H.parentState "") =<< awaitBody
