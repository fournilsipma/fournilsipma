module Types where

import Prelude                    (class Show, class Eq, bind, pure, ($),
                                   (<=<), (<<<))
import Control.Monad.Except       (ExceptT(..))
import Data.Bifunctor             (lmap)
import Data.Either                (Either)
import Data.Foreign               (F, ForeignError(..))
import Data.Foreign.Class         (class Decode, class Encode)
import Data.Foreign.Generic       (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.Generic.Types (Options)
import Data.Generic.Rep           (class Generic)
import Data.Generic.Rep.Eq        (genericEq)
import Data.Generic.Rep.Show      (genericShow)
import Data.Newtype               (class Newtype, unwrap)
import Data.Argonaut.Core         (Json, jsonEmptyObject)
import Data.Argonaut.Decode       (class DecodeJson, decodeJson, getField)
import Data.Argonaut.Encode       (class EncodeJson, (:=), (~>))
import Data.Maybe                 (Maybe(..))
import Data.Tuple                 (Tuple(..))
import Data.MediaType.Common as MediaType
import Network.HTTP.Affjax.Response (class Respondable, ResponseType(..),
                                     fromResponse)


newtype Product = Product
  { name        :: String
  , description :: String
  , poids       :: Int
  -- ^ poids en grammes
  , prix        :: Int
  -- ^ prix en centimes d'euros
  }

newtype Shop = Shop
  { produits :: Array Product
  }

derive instance newtypeProduct :: Newtype Product _
derive instance newtypeShop :: Newtype Shop _

access :: forall a b c . Newtype a b => a -> (b -> c) -> c
access t f = f (unwrap t)
infixl 8 access as ^

instance showProduct :: Show Product where
    show = genericShow

instance showShop :: Show Shop where
    show = genericShow

derive instance genericProduct :: Generic Product _
derive instance genericShop :: Generic Shop _

myOptions :: Options
myOptions = defaultOptions { unwrapSingleConstructors = true }

instance eqProduct :: Eq Product where
    eq x y = genericEq x y

instance eqShop :: Eq Shop where
    eq x y = genericEq x y

instance decodeProduct :: Decode Product where
    decode x = genericDecode myOptions x

instance decodeShop :: Decode Shop where
    decode x = genericDecode myOptions x

instance encodeProduct :: Encode Product where
    encode x = genericEncode myOptions x

instance encodeShop :: Encode Shop where
    encode x = genericEncode myOptions x
