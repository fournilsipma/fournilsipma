module Shop where

import Prelude
import Control.Monad.Aff            (Aff)
import Control.Monad.Eff            (Eff, kind Effect)
import Control.Monad.Eff.Exception  (EXCEPTION, throw)
import Control.Promise              (Promise, toAff)
import CSS                          as CSS
import Data.Argonaut.Encode         (encodeJson)
import Data.Argonaut.Decode         (decodeJson)
import Data.Array                   (mapWithIndex, modifyAt)
import Data.Either                  (Either(..))
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


foreign import data PIKADAY :: Effect
foreign import data Pikaday :: Type
foreign import pikadayNew :: forall eff. String -> Eff (pikaday :: PIKADAY | eff) Pikaday

foreign import data STRIPE :: Effect
foreign import data Stripe :: Type
foreign import data Elements :: Type
foreign import data Card :: Type
foreign import stripeStripe :: forall eff. APIKey -> Eff (stripe :: STRIPE | eff) Stripe
foreign import stripeElements :: forall eff. Stripe -> Eff (stripe :: STRIPE | eff) Elements
foreign import stripeCard :: forall eff. Elements -> Eff (stripe :: STRIPE | eff) Card
foreign import stripeCardMount :: forall eff. Card -> String -> Eff (stripe :: STRIPE | eff) Unit
foreign import stripeCreateToken :: Stripe -> Card -> Promise Unit
type APIKey = String

fournilShopJson :: String
fournilShopJson = "/fournil-produits.json"

type ProductState = { product :: Product, pindex :: Int, quantity :: Int }

type State =
  { produits :: Array ProductState
  , total :: Int
  , processing :: Boolean
  , response :: Maybe ChargeResponseType
  , stripe :: Maybe Stripe
  , card :: Maybe Card
  }

data Query a
  = SetQuantity ProductState String a
  | SubmitForm Event a
  | DiscardResponse a
  | Initialize a

type AppEffects eff = Aff (ajax :: AJAX, console :: CONSOLE, dom :: DOM, exception :: EXCEPTION, pikaday :: PIKADAY, stripe :: STRIPE | eff)

shopUI :: forall eff. Shop -> H.Component HH.HTML Query Unit Unit (AppEffects eff)
shopUI shop = H.lifecycleComponent
  { initialState : const initialState
  , render
  , eval
  , initializer  : Just (H.action Initialize)
  , finalizer    : Nothing
  , receiver     : const Nothing
  }
 where
  initialState :: State
  initialState =
    { produits : mapWithIndex (\i -> \p -> { product : p, pindex: i, quantity : 0 }) (shop^_.produits)
    , total : 0
    , processing : false
    , response : Nothing
    , stripe : Nothing
    , card : Nothing
    }

  render :: State -> H.ComponentHTML Query
  render state = HH.div
    [] $
    maybe [] alertdiv state.response <>
    [ HH.table
      [ HP.classes $ H.ClassName <$>
        [ "fournil-table"
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
          [ HH.text "Quantité" ]
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
    alertdiv res =
      [ HH.div
        [ HP.classes $ H.ClassName <$>
          [ "alert"
          , "alert-dismissible"
          , "fade"
          , "show"
          , case res of
              ResponseDecodeError _ -> "alert-danger"
              ResponseSuccess _ -> "alert-success"
          ]
        ] $
        ( case res of
            ResponseDecodeError err -> [ HH.text $ "Erreur: " <> err ]
            ResponseSuccess cr ->
              [ HH.div
                []
                [ HH.text $ "Votre commande est bien enregistrée. Un email de confirmation a été envoyé à l'adresse " <> cr^_.chargeemail ]
              , HH.div
                []
                [ HH.text $ "Référence de la commande : " <> cr^_.chargeid ]
              ]
        ) <>
        [ HH.button
          [ HP.class_ $ H.ClassName "close"
          , HP.type_ HP.ButtonButton
          , HP.attr (HH.AttrName "data-dismiss") "alert"
          , HE.onClick $ HE.input_ DiscardResponse
          ]
          [ HH.text "×" ]
        ]
      ]

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
      , formdivelement "fournil-form-date" "Date d'enlèvement" Nothing HP.InputText []
      , HH.div
        [ HP.classes $ H.ClassName <$> [ "row", "form-group" ]
        ]
        [ HH.label
          [ HP.for "fournil-card-element"
          , HP.classes $ H.ClassName <$> [ "col-sm-4", "col-form-label" ]
          ]
          [ HH.text "Carte de crédit"
          , HH.img
              [ HP.class_ $ H.ClassName "fournil-card-image"
              , HP.alt "Sécurisé par Stripe"
              , HP.src (shop^_.stripeImage)
              ]
          ]
        , HH.div
          [ HP.class_ $ H.ClassName "col-sm-8" ]
          [ HH.div
            [ HP.id_ "fournil-card-element"
            , HP.class_ $ H.ClassName "form-control"
            ]
            []
          , HH.div
            [ HP.class_ $ H.ClassName "fournil-card-error"
            , HP.id_ "fournil-card-errors"
            , HPA.role "alert"
            , HC.style do
              CSS.display CSS.displayNone
            ]
            []
          ]
        ]
      , HH.input
        [ HP.id_ "fournil-card-tokenid"
        , HP.type_ HP.InputHidden
        , HP.ref (H.RefLabel "fournil-card-tokenid")
        ]
      , HH.div
        [ HP.class_ $ H.ClassName "form-group" ]
        [ HH.div
          [ HP.class_ $ H.ClassName "form-submit" ]
          [ HH.button
            ( [ HP.classes $ H.ClassName <$>
                [ "btn"
                , "btn-outline-success"
                , "btn-lg"
                , "btn-block"
                ]
              , HP.type_ HP.ButtonSubmit
              ] <> if st.total == 0 || st.processing then [ HP.disabled true ] else []
            ) $
            [ HH.text "Passer la commande!" ]
            <> if st.processing then loaderelements else []
          ]
        ]
      ]

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
          [ "col-sm-4"
          , "col-form-label"
          ]
        ]
        [ HH.text desc ]
      , HH.div
        [ HP.classes $ H.ClassName <$> [ "col-sm-8" ]
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
     stripe <- maybe (H.liftEff $ throw "Erreur d'initialisation de Stripe") pure (state.stripe)
     card <- maybe (H.liftEff $ throw "Erreur d'initialisation de Stripe.Card") pure (state.card)
     H.liftAff $ toAff (stripeCreateToken stripe card)
     name <- getRefValue "fournil-form-nom"
     email <- getRefValue "fournil-form-email"
     phone <- getRefValue "fournil-form-tel"
     date <- getRefValue "fournil-form-date"
     tokenid <- getRefValue "fournil-card-tokenid"
     let formdata = ChargeForm
           { "name"     : name
           , "email"    : email
           , "phone"    : phone
           , "date"     : date
           , "articles" : map (\ps -> Article { product : ps.product, quantity : ps.quantity }) state.produits
           , "tokenid"  : tokenid
           }
     res <- H.liftAff $ post (shop^_.serverUrl <> shop^_.chargeUrl) (encodeJson formdata)
     case decodeJson (res.response) :: Either String ChargeResponse of
       Left err -> H.modify (_ { processing = false, response = Just $ ResponseDecodeError err })
       Right cr -> H.modify (_
          { processing = false
          , response = Just $ ResponseSuccess cr
          , produits = initialState.produits
          , total = initialState.total
          })
     pure next
  eval (DiscardResponse next) = do
    H.modify (_ { response = Nothing })
    pure next
  eval (Initialize next) = do
    _ <- H.liftEff $ pikadayNew "fournil-form-date"
    stripe <- H.liftEff $ stripeStripe (shop^_.stripeApiKey)
    elements <- H.liftEff $ stripeElements stripe
    card <- H.liftEff $ stripeCard elements
    H.modify (_ { stripe = Just stripe, card = Just card })
    H.liftEff $ stripeCardMount card "#fournil-card-element"
    pure next

  getRefValue label = do
    me <- H.getHTMLElementRef (H.RefLabel label)
    case me of
      Just e -> case fromHTMLElement e of
        Just ie -> H.liftEff $ value ie
        Nothing -> H.liftEff $ throw $ "La conversion de " <> label <> " a échoué."
      Nothing -> H.liftEff $ throw $ "Pas de référence '" <> label <> "' trouvée."
