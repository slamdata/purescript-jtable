module Main where

import Prelude 
import Data.Void
import Data.Tuple
import Data.Either
import Data.List (toList)

import Control.Bind
import Control.Monad.Eff
import Control.Alternative

import DOM

import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window

import Data.Argonaut.Core (Json())
import Data.Argonaut.Parser (jsonParser)
import Data.Json.JTable (renderJTable, jTableOptsDefault, bootstrapStyle)

import qualified Data.StrMap as StrMap

import Halogen
import Halogen.Signal
import Halogen.Component

import qualified Halogen.HTML as H
import qualified Halogen.HTML.Attributes as A
import qualified Halogen.HTML.Events as A
import qualified Halogen.HTML.Events.Forms as A
import qualified Halogen.HTML.CSS as C

appendToBody :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = document globalWindow >>= (body >=> flip appendChild e)

ui :: forall m eff. (Alternative m) => Component m String String
ui = render <$> (input `startingAt` "")
  where
  render :: String -> H.HTML (m String)
  render json = 
    H.div [ A.class_ (A.className "container") ]
          [ H.h1_ [ H.text "purescript-jtable demo" ]
          , H.p_ [ H.text "Paste some JSON:" ]
          , H.p_ [ H.textarea [ A.class_ (A.className "form-control") 
                              , A.value json 
                              , A.onInput (A.input id)
                              ] [] ]
          , H.h2_ [ H.text "Output" ]
          , either H.text (absurd <$>) table
          ]
    where
    table :: Either String (H.HTML Void)
    table = renderJTable (jTableOptsDefault { style = bootstrapStyle }) <$> jsonParser json

main = do
  Tuple node _ <- runUI ui
  appendToBody node
