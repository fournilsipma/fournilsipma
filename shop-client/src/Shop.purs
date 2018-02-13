module Shop where

import Prelude
import Control.Monad.Aff            (Aff)
import Control.Monad.Eff            (Eff, kind Effect)
import Control.Monad.Eff.Exception  (EXCEPTION, throw)
import CSS                          as CSS
import Data.Argonaut.Encode         (encodeJson)
import Data.Argonaut.Decode         (decodeJson)
import Data.Array                   (index)
import Data.Either                  (Either(..))
import Data.Int                     (floor, toNumber, fromString)
import Data.Maybe                   (Maybe(..), maybe)
import DOM                          (DOM)
import DOM.Event.Event              (preventDefault)
import DOM.Event.Types              (Event)
import DOM.HTML.HTMLInputElement    (value)
import Halogen                      as H
import Halogen.HTML                 as HH
import Halogen.HTML.Events          as HE
import Halogen.HTML.Properties      as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.HTML.CSS             as HC
import Network.HTTP.Affjax          (AJAX, post, get)
import Control.Monad.Eff.Console    (CONSOLE)
import Text.Format                  (format, precision)
import Types


fournilShopJson :: String
fournilShopJson = "/fournil-produits.json"

type State =
  {
  }

data Query a
  = Initialize a

type AppEffects eff = Aff (ajax :: AJAX, console :: CONSOLE, dom :: DOM, exception :: EXCEPTION | eff)

shopUI :: forall eff. Shop -> H.Component HH.HTML Query Unit Unit (AppEffects eff)
shopUI shop = H.component
  { initialState : const initialState
  , render
  , eval
  , receiver     : const Nothing
  }
 where
  initialState :: State
  initialState =
    {
    }

  render :: State -> H.ComponentHTML Query
  render state = HH.table
    [ HP.classes $ H.ClassName <$>
      [ "fournil-shop"
      , "table"
      ]
    ]
    [ HH.thead
      []
      [ HH.th
        []
        [ HH.text "Désignation" ]
      , HH.th
        []
        [ HH.text "Nombre" ]
      , HH.th
        []
        [ HH.text "Poids" ]
      , HH.th
        []
        [ HH.text "Prix du pain" ]
      , HH.th
        []
        [ HH.text "Total" ]
      ]
    , HH.tbody
      [] $
      map shopelement $ shop^_.produits
    ]
   where
    shopelement produit = HH.tr
      []
      [ HH.td
        [ HP.colSpan 2 ]
        [ HH.span
          [ HP.class_ $ H.ClassName "font-weight-bold" ]
          [ HH.text $ produit^_.name ]
        , HH.div
          [ HP.class_ $ H.ClassName "font-italic" ]
          [ HH.text $ produit^_.description ]
        ]
      , HH.td
        []
        [ HH.text $ show (produit^_.poids) <> "g" ]
      , HH.td
        []
        [ HH.text $ format (precision 2) (toNumber (produit^_.prix) / 100.0) <> "€" ]
      , HH.td
        []
        []
      ]

  eval :: Query ~> H.ComponentDSL State Query Unit (AppEffects eff)
  eval = case _ of
    Initialize next -> pure next
