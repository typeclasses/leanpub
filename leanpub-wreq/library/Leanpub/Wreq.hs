{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}

{-# LANGUAGE BlockArguments, DerivingVia, RecordWildCards #-}

{- | Example:

> do
>   session <- newAPISession
>
>   let
>     key = ApiSecretKey (Data.Text.pack "jMeaxrJgzfghanEWhjE")
>     code = CouponCode (Data.Text.pack "kAmr43mva91ndApQ")
>     slug = BookSlug (Data.Text.pack "finding-success-in-haskell")
>     uses = CouponMaxUses 2
>
>   runLeanpub session (Just key) (createFreeBookCoupon slug code uses)

-}

module Leanpub.Wreq
  (
  -- * The Leanpub monad
    Leanpub, runLeanpub, runLeanpub'

  -- * Leanpub actions
  , getBookSummary
  , getBookSalesSummary
  , createFreeBookCoupon

  -- * Wreq
  , wreqGet, wreqPost, wreqGetAeson, wreqPostAeson
  , WreqResponse, Path, Extension
  , QueryParam, FormParam (..), Session, newAPISession

  ) where

-- aeson
import qualified Data.Aeson

-- base
import Control.Monad hiding (fail)
import Control.Monad.IO.Class
import Control.Monad.Fail
import Data.Maybe
import Prelude hiding (fail)

-- bytestring
import qualified Data.ByteString.Char8
import qualified Data.ByteString
import qualified Data.ByteString.Lazy

-- leanpub-concepts
import Leanpub.Concepts

-- lens
import Control.Lens ((&), (.~), (^.))

-- text
import           Data.Text (Text)
import qualified Data.Text

-- time
import           Data.Time (Day)
import qualified Data.Time

-- transformers
import Control.Monad.Trans.Reader

-- wreq
import           Network.Wreq         (FormParam (..))
import qualified Network.Wreq
import           Network.Wreq.Session (Session, newAPISession)
import qualified Network.Wreq.Session

text :: String -> Text
text = Data.Text.pack

ascii :: String -> Data.ByteString.ByteString
ascii = Data.ByteString.Char8.pack

------------------------------------------------------------

data Context =
  Context
    { contextSession :: Session
    , contextKeyMaybe :: Maybe ApiSecretKey
    }

newtype Leanpub a = Leanpub (Context -> IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail)
    via ReaderT Context IO

runLeanpub :: Session -> Maybe ApiSecretKey -> Leanpub a -> IO a
runLeanpub contextSession contextKeyMaybe (Leanpub f) =
    f Context{..}

runLeanpub' :: Maybe ApiSecretKey -> Leanpub a -> IO a
runLeanpub' contextKeyMaybe (Leanpub f) =
    newAPISession >>= \contextSession -> f Context{..}

requireKey :: Leanpub ()
requireKey =
    Leanpub \Context{..} ->
        when (isNothing contextKeyMaybe) (fail "API key is required.")

------------------------------------------------------------

type WreqResponse = Network.Wreq.Response Data.ByteString.Lazy.ByteString

type Path = [Text]

type Extension = Text

type QueryParam = (Text, Text)

urlBase :: Text
urlBase = text "https://leanpub.com/"

makeUrl :: Path -> Extension -> String
makeUrl xs ext =
    f xs ++ "." ++ Data.Text.unpack ext
  where
    f =
        Data.Text.unpack
        . Data.Text.intercalate (text "/")
        . (urlBase :)

wreqDefaults :: Network.Wreq.Options
wreqDefaults = Network.Wreq.defaults

authGetParam :: ApiSecretKey -> (Text, Text)
authGetParam (ApiSecretKey key) = (text "api_key", key)

authFormParam :: ApiSecretKey -> FormParam
authFormParam (ApiSecretKey key) = ascii "api_key" := key

wreqGet :: Path -> Extension -> [QueryParam] -> Leanpub WreqResponse
wreqGet path extension params =
    Leanpub \Context{..} ->
      let
          url = makeUrl path extension

          params' =
              maybe id (\key -> (authGetParam key :)) contextKeyMaybe
              params

          opts =
              wreqDefaults
                  & Network.Wreq.params .~ params'
      in
          Network.Wreq.Session.getWith opts contextSession url

wreqPost :: Path -> Extension -> [FormParam] -> Leanpub WreqResponse
wreqPost path extension params =
    Leanpub \Context{..} ->
        let
            url = makeUrl path extension

            params' =
                maybe id (\key -> (authFormParam key :)) contextKeyMaybe
                params

            opts =
                wreqDefaults
        in
            Network.Wreq.Session.postWith opts contextSession url params'

------------------------------------------------------------

extJson :: Extension
extJson = text "json"

wreqBodyAeson :: MonadFail m => WreqResponse -> m Data.Aeson.Value
wreqBodyAeson =
    either fail return
    . Data.Aeson.eitherDecode
    . (^. Network.Wreq.responseBody)

wreqGetAeson :: Path -> [QueryParam] -> Leanpub Data.Aeson.Value
wreqGetAeson path params =
    wreqGet path extJson params >>= wreqBodyAeson

wreqPostAeson :: Path -> [FormParam] -> Leanpub Data.Aeson.Value
wreqPostAeson path params =
    wreqPost path extJson params >>= wreqBodyAeson

------------------------------------------------------------

getBookSummary :: BookSlug -> Leanpub Data.Aeson.Value
getBookSummary (BookSlug slug) =
    wreqGetAeson [slug] []

getBookSalesSummary :: BookSlug -> Leanpub Data.Aeson.Value
getBookSalesSummary (BookSlug slug) =
  do
    requireKey
    wreqGetAeson [slug, text "sales"] []

createFreeBookCoupon :: BookSlug -> CouponCode -> CouponMaxUses
    -> Leanpub Data.Aeson.Value
createFreeBookCoupon (BookSlug slug) (CouponCode code) uses =
  do
    requireKey
    start <- liftIO getToday

    let
        params =
          catMaybes
            [ Just (ascii "coupon[coupon_code]" := code)
            , Just (ascii "coupon[start_date]" := formatDay start)
            , Just (
                ascii "coupon[package_discounts_attributes][0][package_slug]"
                := "book")
            , Just (
                ascii "coupon[package_discounts_attributes][0][discounted_price]"
                := "0.00")
            , case uses of
                CouponUseUnlimited -> Nothing
                CouponMaxUses n -> Just (
                  ascii "coupon[max_uses]" := toInteger n)
            ]

    wreqPostAeson [slug, text "coupons"] params

getToday :: IO Day
getToday = Data.Time.utctDay <$> Data.Time.getCurrentTime

formatDay :: Day -> String
formatDay = Data.Time.formatTime Data.Time.defaultTimeLocale "%Y-%m-%d"
