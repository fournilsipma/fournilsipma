module Main where

import Prelude                     (Unit, bind, discard, pure, show, unit,
                                   ($), (=<<))
import Control.Monad.Eff           (Eff)
import Control.Monad.Eff.Class     (liftEff)
import Control.Monad.Eff.Console   (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Eff.Now       (NOW, nowDateTime)
import Control.Monad.Except        (runExcept)
import Data.Foldable               (traverse_)
import Data.Either                 (Either(..))
import DOM.Node.ParentNode         (QuerySelector(..))
import Halogen.Aff                 as HA
import Halogen.VDom.Driver         (runUI)
import Types
import Shop                        as S
import Network.HTTP.Affjax         (AJAX, get)
import Data.Foreign.Generic        (genericDecodeJSON)


main :: Eff (HA.HalogenEffects ( console :: CONSOLE, ajax :: AJAX, now :: NOW, pikaday :: S.PIKADAY )) Unit
main = HA.runHalogenAff do
    HA.awaitLoad
    res <- get S.fournilShopJson
    p <- liftEff $ decodeShop (res.response)
    d <- liftEff nowDateTime
    traverse_ (runUI (S.shopUI p d) unit) =<< HA.selectElement (QuerySelector "#fournil-shop-app")

decodeShop :: forall e. String -> Eff ( exception :: EXCEPTION | e) Shop
decodeShop json = case runExcept (genericDecodeJSON myOptions json) of
    Right p -> pure p
    Left err -> throw (show err)
