module Main where

import Data.Void
import Data.Tuple
import Data.Either

import Control.Bind
import Control.Monad.Eff
import Control.Alternative

import DOM

import Data.Bifunctor (bimap)

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

appendToBody :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Unit
appendToBody e = document globalWindow >>= (body >=> flip appendChild e)

ui :: forall p m eff. (Alternative m) => Component p m String String
ui = component (render <$> (input `startingAt` ""))
  where
  render :: String -> H.HTML p (m String)
  render json = 
    H.div [ A.class_ (A.className "container") ]
          [ H.h1_ [ H.text "purescript-jtable demo" ]
          , H.p_ [ H.text "Paste some JSON:" ]
          , H.p_ [ H.textarea [ A.class_ (A.className "form-control") 
                              , A.value json 
                              , A.onInput (A.input id)
                              , A.style (A.styles $ StrMap.fromList 
                                          [ Tuple "font-family" "monospace"
                                          , Tuple "height" "200px"
                                          ])
                              ] [] ]
          , H.h2_ [ H.text "Output" ]
          , either H.text (bimap absurd absurd) table
          ]
    where
    table :: Either String (H.HTML Void Void)
    table = renderJTable (jTableOptsDefault { style = bootstrapStyle }) <$> jsonParser json

main = do
  Tuple node _ <- runUI ui
  appendToBody node