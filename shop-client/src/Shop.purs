module Shop where

import Prelude
import Control.Monad.Aff            (Aff)
import Control.Monad.Eff            (Eff, kind Effect)
import Control.Monad.Eff.Exception  (EXCEPTION, throw)
import CSS                          as CSS
import Data.Argonaut.Encode         (encodeJson)
import Data.Argonaut.Decode         (decodeJson)
import Data.Array                   (mapWithIndex, modifyAt)
import Data.DateTime                (DateTime, adjust)
import Data.Time.Duration           (Days(..))
import Data.DateTime.Locale         (LocalValue(..), LocalDateTime)
import Data.Interval.Duration       (Duration)
import Data.Either                  (Either(..))
import Data.Formatter.DateTime      as D
import Data.Int                     (toNumber, fromString)
import Data.List                    (List(Nil), (:))
import Data.Maybe                   (Maybe(..), maybe)
import Data.Foldable                (foldl)
import DOM                          (DOM)
import DOM.Classy.HTMLElement       (fromHTMLElement)
import DOM.Event.Event              (preventDefault)
import DOM.Event.Types              (Event)
import DOM.HTML.HTMLInputElement    (value)
import Halogen                      as H
import Halogen.HTML                 as HH
import Halogen.HTML.Core            (PropName(..))
import Halogen.HTML.Events          as HE
import Halogen.HTML.Properties      as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.HTML.CSS             as HC
import Network.HTTP.Affjax          (AJAX, post)
import Control.Monad.Eff.Console    (CONSOLE)
import Text.Format                  (format, precision)
import Types


fournilShopJson :: String
fournilShopJson = "/fournil-produits.json"

offset :: Days
offset = Days 2.0

serverUrl :: String
serverUrl = "http://127.0.0.1:8080"

chargeUrl :: String
chargeUrl = "/charge"

type ProductState = { product :: Product, pindex :: Int, quantity :: Int }

type State =
  { produits :: Array ProductState
  , total :: Int
  , processing :: Boolean
  , response :: ChargeResponseType
  }

data Query a
  = SetQuantity ProductState String a
  | SubmitForm Event a

type AppEffects eff = Aff (ajax :: AJAX, console :: CONSOLE, dom :: DOM, exception :: EXCEPTION | eff)

shopUI :: forall eff. Shop -> LocalDateTime -> H.Component HH.HTML Query Unit Unit (AppEffects eff)
shopUI shop date = H.component
  { initialState : const initialState
  , render
  , eval
  , receiver     : const Nothing
  }
 where
  initialState :: State
  initialState =
    { produits : mapWithIndex (\i -> \p -> { product : p, pindex: i, quantity : 0 }) (shop^_.produits)
    , total : 0
    , processing : false
    , response : NoResponse
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
    ] <> [contactelement state]
   where
    contactelement st = HH.form
      [ HP.classes $ H.ClassName <$>
          [ "fournil-form"
          ]
      , HP.method HP.POST
      , HP.action "#"
      , HE.onSubmit (HE.input SubmitForm)
      ]
      [ formdivelement "fournil-form-nom" "Nom" Nothing HP.InputText []
      , formdivelement "fournil-form-email" "Email" (Just "moi@example.com") HP.InputEmail []
      , formdivelement "fournil-form-tel" "Téléphone" (Just "02 32 11 11 11") HP.InputTel []
      , formdivelement "fournil-form-date" "Date" Nothing HP.InputDate $
        maybe [] (\v -> [ HP.value v, HP.prop (PropName "min") v ]) mlocaldate
      , HH.div
        [ HP.class_ $ H.ClassName "form-group" ]
        [ HH.div
          [ HP.class_ $ H.ClassName "form-submit" ]
          [ HH.button
            ( [ HP.classes $ H.ClassName <$>
                [ "btn"
                , "btn-success"
                ]
              , HP.type_ HP.ButtonSubmit
              ] <> if st.total == 0 || st.processing then [ HP.disabled true ] else []
            ) $
            [ HH.text "Passer la commande!" ]
            <> if st.processing then loaderelements else []
          ]
        ]
      ]

    simpledate (LocalValue _ sdate) = sdate

    formatter
      = D.YearFull
      : D.Placeholder "-"
      : D.MonthTwoDigits
      : D.Placeholder "-"
      : D.DayOfMonthTwoDigits
      : Nil

    mdatewithoffset :: Maybe DateTime
    mdatewithoffset = adjust offset (simpledate date)

    mlocaldate = D.format formatter <$> mdatewithoffset

    loaderelements =
      [ HH.span
        [ HP.class_ $ H.ClassName "fournil-spin" ]
        []
      ]

    formdivelement id desc mplaceholder inputtype attributes = HH.div
      [ HP.classes $ H.ClassName <$>
        [ "form-group"
        , "row"
        ]
      ]
      [ HH.label
        [ HP.classes $ H.ClassName <$>
          [ "col-sm-2"
          , "col-form-label"
          ]
        ]
        [ HH.text desc ]
      , HH.div
        [ HP.classes $ H.ClassName <$> [ "col-sm-10" ]
        ]
        [ HH.input $
          [ HP.type_ inputtype
          , HP.class_ $ H.ClassName "form-control"
          , HP.required true
          , HP.id_ id
          , HP.ref (H.RefLabel id)
          ]
          <> maybe [] (\p -> [ HP.placeholder p ]) mplaceholder
          <> attributes
        ]
      ]

    shopelement ps = HH.tr
      []
      [ HH.td
        []
        [ HH.span
          [ HP.class_ $ H.ClassName "font-weight-bold" ]
          [ HH.text $ ps.product^_.nom ]
        , HH.div
          [ HP.class_ $ H.ClassName "font-italic" ]
          [ HH.text $ ps.product^_.description ]
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
        [ HH.text $ show (ps.product^_.poids) <> "g" ]
      , HH.td
        []
        [ HH.text $ showPrice (ps.product^_.prix) ]
      , HH.td
        []
        [ HH.text $ showPrice (ps.quantity * ps.product^_.prix) ]
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
            Just ps -> st { produits = ps, total = foldl (\t -> \ps -> t + ps.quantity * ps.product^_.prix) 0 ps }
        pure next
      Nothing -> pure next
        -- ^ TODO: add error message
  eval (SubmitForm event next) = do
     H.liftEff $ preventDefault event
     state <- H.get
     H.modify (_ { processing = true })
     -- TODO
     name <- getRefValue "fournil-form-nom"
     email <- getRefValue "fournil-form-email"
     phone <- getRefValue "fournil-form-tel"
     date <- getRefValue "fournil-form-date"
     let formdata = ChargeForm
           { "name"     : name
           , "email"    : email
           , "phone"    : phone
           , "date"     : date
           , "articles" : map (\ps -> Article { product : ps.product, quantity : ps.quantity }) state.produits
           }
     res <- H.liftAff $ post (serverUrl <> chargeUrl) (encodeJson formdata)
     case decodeJson (res.response) :: Either String ChargeResponse of
       Left err -> H.modify (_ { processing = false, response = ResponseDecodeError err })
       Right cr -> H.modify (_ { processing = false, response = ResponseSuccess cr })
     pure next

  getRefValue label = do
    me <- H.getHTMLElementRef (H.RefLabel label)
    case me of
      Just e -> case fromHTMLElement e of
        Just ie -> H.liftEff $ value ie
        Nothing -> H.liftEff $ throw $ "La conversion de " <> label <> " a échoué."
      Nothing -> H.liftEff $ throw $ "Pas de référence '" <> label <> "' trouvée."
