module Main where

import Prelude
import Data.Either
import Data.Functor.Coproduct
import Data.List (toList)
import Data.Tuple
import Data.Void

import Control.Bind
import Control.Monad.Eff
import Control.Monad.Eff.Exception (EXCEPTION(), throwException)
import qualified Control.Monad.Aff as Aff
import qualified Control.Monad.Aff.AVar as Aff
import Control.Plus

import Data.Argonaut.Core (Json(), jsonEmptyArray)
import Data.Argonaut.Parser (jsonParser)
import qualified Data.Json.JTable as J

import qualified Data.StrMap as StrMap

import Halogen
import Halogen.Util
import Halogen.Component

import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Properties.Indexed as P
import qualified Halogen.HTML.Events.Indexed as E

type DemoState = String
data DemoQuery a = SetJsonText String a

type DemoSlot = Unit
type DemoInstalledState g = InstalledState DemoState Json DemoQuery J.JTableQuery g DemoSlot
type DemoComponent g = Component (DemoInstalledState g) (Coproduct DemoQuery (ChildF DemoSlot J.JTableQuery)) g
type DemoRender g = RenderParent DemoState Json DemoQuery J.JTableQuery g DemoSlot
type DemoEval g = EvalParent DemoQuery DemoState Json DemoQuery J.JTableQuery g DemoSlot

ui :: forall g. (Plus g) => J.JTableOpts -> DemoComponent g
ui opts = parentComponent' render eval (\_ -> pure unit)
  where
    render :: DemoRender g
    render jsonString =
      H.div
        [ P.class_ $ H.className "container" ]
        [ H.h1_ [ H.text "purescript-jtable demo" ]
        , H.p_ [ H.text "Paste some JSON:" ]
        , H.p_
            [ H.textarea
                [ P.class_ $ H.className "form-control"
                , P.value jsonString
                , E.onValueInput $ E.input SetJsonText
                ]
            ]
        , H.h2_ [ H.text "Output" ]
        , H.slot unit \_ ->
            { component : J.jtableComponent opts
            , initialState : jsonEmptyArray
            }
        ]

    eval :: DemoEval g
    eval (SetJsonText jsonString next) = do
      query unit <<< action <<< J.SetJson $
        case jsonParser jsonString of
          Left _ -> jsonEmptyArray
          Right json -> json
      pure next

type DemoEffects =
  ( dom :: DOM.DOM
  , avar :: Aff.AVAR
  , err :: EXCEPTION
  )

main :: Eff DemoEffects Unit
main =
  Aff.runAff throwException (const (pure unit)) $ do
    app <- runUI (ui J.jTableOptsDefault) (installedState "")
    appendToBody app.node
