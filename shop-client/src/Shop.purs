module Shop where

import Prelude
import Control.Monad.Aff            (Aff)
import Control.Monad.Eff            (Eff, kind Effect)
import Control.Monad.Eff.Exception  (EXCEPTION, throw)
import CSS                          as CSS
import Data.Argonaut.Encode         (encodeJson)
import Data.Argonaut.Decode         (decodeJson)
import Data.Array                   (mapWithIndex, modifyAt)
import Data.Int                     (toNumber, fromString)
import Data.Maybe                   (Maybe(..))
import Data.Foldable                (foldl)
import DOM                          (DOM)
import DOM.Event.Event              (preventDefault)
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

type ProductState = { produit :: Product, pindex :: Int, quantity :: Int }

type State = { produits :: Array ProductState, total :: Int }

data Query a
  = SetQuantity ProductState String a

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
    { produits : mapWithIndex (\i -> \p -> { produit : p, pindex: i, quantity : 0 }) (shop^_.produits)
    , total : 0
    }

  render :: State -> H.ComponentHTML Query
  render state = HH.div
    [] $
    [ HH.table
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
        []
        (map shopelement (state.produits) <> [totalelement state])
      ]
    ]
   where
    shopelement ps = HH.tr
      []
      [ HH.td
        []
        [ HH.span
          [ HP.class_ $ H.ClassName "font-weight-bold" ]
          [ HH.text $ ps.produit^_.name ]
        , HH.div
          [ HP.class_ $ H.ClassName "font-italic" ]
          [ HH.text $ ps.produit^_.description ]
        ]
      , HH.td
        []
        [ HH.input
          [ HP.value $ show ps.quantity
          , HP.type_ HP.InputNumber
          , HP.class_ $ H.ClassName "form-control"
          , HE.onValueInput (HE.input $ SetQuantity ps)
          ]
        ]
      , HH.td
        []
        [ HH.text $ show (ps.produit^_.poids) <> "g" ]
      , HH.td
        []
        [ HH.text $ showPrice (ps.produit^_.prix) ]
      , HH.td
        []
        [ HH.text $ showPrice (ps.quantity * ps.produit^_.prix) ]
      ]
    totalelement st = HH.tr
      []
      [ HH.td
        [ HP.class_ $ H.ClassName "font-weight-bold"
        , HP.colSpan 4
        ]
        [ HH.div
          []
          [ HH.text "TOTAL" ]
        , HH.a
          [ HP.classes $ H.ClassName <$>
            [ "btn", "btn-success" ] <> if st.total == 0 then ["disabled"] else []
            -- ^ TODO
          , HP.href "#fournil-commande"
          ]
          [ HH.text "Commander" ]
        ]
      , HH.td
        [ HP.class_ $ H.ClassName "font-weight-bold" ]
        [ HH.text $ showPrice st.total ]
      ]

  showPrice :: Int -> String
  showPrice p = format (precision 2) (toNumber p / 100.0) <> "€"

  eval :: Query ~> H.ComponentDSL State Query Unit (AppEffects eff)
  eval (SetQuantity ps q next) = do
    case fromString q of
      Just q' -> do
        H.modify $ \st -> case modifyAt (ps.pindex) (\r -> r { quantity = q' }) (st.produits) of
            Nothing -> st
              -- ^ TODO: add error message
            Just ps -> st { produits = ps, total = foldl (\t -> \ps -> t + ps.quantity * ps.produit^_.prix) 0 ps }
        pure next
      Nothing -> pure next
        -- ^ TODO: add error message
