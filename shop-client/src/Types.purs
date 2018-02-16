module Types where

import Prelude                    (class Show, class Eq, bind, pure, ($))
import Data.Foreign.Class         (class Decode, class Encode)
import Data.Foreign.Generic       (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Generic.Types (Options)
import Data.Generic.Rep           (class Generic)
import Data.Generic.Rep.Eq        (genericEq)
import Data.Generic.Rep.Show      (genericShow)
import Data.Newtype               (class Newtype, unwrap)
import Data.Argonaut.Core         (jsonEmptyObject)
import Data.Argonaut.Decode       (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.Encode       (class EncodeJson, (:=), (~>))
import Network.HTTP.Affjax.Response (class Respondable, ResponseType(..),
                                     fromResponse)


access :: forall a b c . Newtype a b => a -> (b -> c) -> c
access t f = f (unwrap t)
infixl 8 access as ^

myOptions :: Options
myOptions = defaultOptions { unwrapSingleConstructors = true }

-- Product
newtype Product = Product
  { nom         :: String
  , description :: String
  , poids       :: Int
  -- ^ poids en grammes
  , prix        :: Int
  -- ^ prix en centimes d'euros
  }

derive instance newtypeProduct :: Newtype Product _

instance showProduct :: Show Product where
  show = genericShow

derive instance genericProduct :: Generic Product _

instance eqProduct :: Eq Product where
  eq x y = genericEq x y

instance decodeProduct :: Decode Product where
  decode x = genericDecode myOptions x

instance encodeProduct :: Encode Product where
  encode x = genericEncode myOptions x

instance encodeJsonProduct :: EncodeJson Product where
  encodeJson (Product p)
    = "nom" := p.nom
    ~> "description" := p.description
    ~> "poids" := p.poids
    ~> "prix" := p.prix
    ~> jsonEmptyObject

-- Shop
newtype Shop = Shop
  { produits :: Array Product
  }

derive instance newtypeShop :: Newtype Shop _

instance showShop :: Show Shop where
  show = genericShow

derive instance genericShop :: Generic Shop _

instance eqShop :: Eq Shop where
  eq x y = genericEq x y

instance decodeShop :: Decode Shop where
  decode x = genericDecode myOptions x

instance encodeShop :: Encode Shop where
  encode x = genericEncode myOptions x

-- Article
newtype Article = Article
  { product  :: Product
  , quantity :: Int
  }

derive instance newtypeArticle :: Newtype Article _

instance showArticle :: Show Article where
  show = genericShow

derive instance genericArticle :: Generic Article _

instance eqArticle :: Eq Article where
  eq x y = genericEq x y

instance decodeArticle :: Decode Article where
  decode x = genericDecode myOptions x

instance encodeArticle :: Encode Article where
  encode x = genericEncode myOptions x

instance encodeJsonArticle :: EncodeJson Article where
  encodeJson (Article p)
    = "product" := p.product
    ~> "quantity" := p.quantity
    ~> jsonEmptyObject

-- ChargeForm
newtype ChargeForm = ChargeForm
  { name     :: String
  , email    :: String
  , phone    :: String
  , date     :: String
  -- ^ TODO: use Date type
  , articles :: Array Article
  }

derive instance newtypeChargeForm :: Newtype ChargeForm _

instance showChargeForm :: Show ChargeForm where
  show = genericShow

derive instance genericChargeForm :: Generic ChargeForm _

instance eqChargeForm :: Eq ChargeForm where
  eq x y = genericEq x y

instance decodeChargeForm :: Decode ChargeForm where
  decode x = genericDecode myOptions x

instance encodeChargeForm :: Encode ChargeForm where
  encode x = genericEncode myOptions x

instance encodeJsonChargeForm :: EncodeJson ChargeForm where
  encodeJson (ChargeForm p)
    = "name" := p.name
    ~> "email" := p.email
    ~> "phone" := p.phone
    ~> "date" := p.date
    ~> "articles" := p.articles
    ~> jsonEmptyObject

-- ChargeResponse
newtype ChargeResponse = ChargeResponse
  { chargeid     :: String
  , chargeamount :: String
  }

derive instance newtypeChargeResponse :: Newtype ChargeResponse _

instance decodeChargeResponse :: Decode ChargeResponse where
  decode x = genericDecode myOptions x

instance encodeChargeResponse :: Encode ChargeResponse where
  encode x = genericEncode myOptions x

derive instance genericChargeResponse :: Generic ChargeResponse _

instance showChargeResponse :: Show ChargeResponse where
  show = genericShow

instance eqChargeResponse :: Eq ChargeResponse where
  eq x y = genericEq x y

instance decodeJsonChargeResponse :: DecodeJson ChargeResponse where
  decodeJson x = do
    obj <- decodeJson x
    chargeid <- obj .? "chargeid"
    chargeamount <- obj .? "chargeamount"
    pure $ ChargeResponse
      { chargeid : chargeid
      , chargeamount : chargeamount
      }

data ChargeResponseType
  = NoResponse
  | ResponseDecodeError String
  | ResponseSuccess ChargeResponse
