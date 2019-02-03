{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}

{-# LANGUAGE BlockArguments, DerivingVia, LambdaCase, RecordWildCards #-}

module Leanpub.Wreq
  (
  -- * The Leanpub monad
    Leanpub (..), Context (..)

  -- * Configuration
  , runLeanpub, Config, configSession, configKey, configKeyFile

  -- * Leanpub actions
  , getBookSummary
  , getBookSalesSummary
  , createFreeBookCoupon
  , createManyFreeBookCoupons

  -- * Wreq
  , wreqGet, wreqPost, wreqGetAeson, wreqPostAeson, wreqPostAeson_
  , WreqResponse, Path, Extension
  , QueryParam, FormParam (..), Session, newAPISession

  ) where

-- aeson
import qualified Data.Aeson

-- base
import Control.Monad hiding (fail, foldM)
import Control.Monad.IO.Class
import Control.Monad.Fail
import qualified Data.List
import Data.Maybe
import Data.Monoid
import Numeric.Natural
import Prelude hiding (fail)

-- bytestring
import qualified Data.ByteString.Char8
import qualified Data.ByteString
import qualified Data.ByteString.Lazy

-- leanpub-concepts
import Leanpub.Concepts

-- lens
import Control.Lens ((&), (.~), (^.))

-- rando
import System.Random.Pick (pickOne)

-- text
import           Data.Text (Text)
import qualified Data.Text
import qualified Data.Text.IO

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

{- | There are two ways to run a 'Leanpub' action:

1. Create a 'Context' and then apply the newtyped
   @Context -> IO a@ function directly.

2. Create a 'Config' and then apply the 'runLeanpub' function.
   This approach is likely more convenient, because it can do
   some things automatically like creating the 'Session' and
   reading your API key from a file. -}

newtype Leanpub a = Leanpub (Context -> IO a)
  deriving ( Functor, Applicative, Monad, MonadIO, MonadFail )
    via ReaderT Context IO

requireKey :: Leanpub ()
requireKey =
    Leanpub \Context{..} ->
        when (isNothing contextKeyMaybe) (fail "API key is required.")

------------------------------------------------------------

runLeanpub :: Config -> Leanpub a -> IO a
runLeanpub (Config config) (Leanpub action) =
    createContext (config baseConfigData) >>= action

createContext :: ConfigData -> IO Context
createContext ConfigData{..} =
  do
    contextSession <-
        case configData_session of
            Nothing -> newAPISession
            Just x -> return x

    contextKeyMaybe <-
        case configData_key of
            KeyConfig_Pure x  -> return (Just x)
            KeyConfig_File x  -> Just <$> readKeyFile x
            KeyConfig_Nothing -> return Nothing

    return Context{..}

{- | Construct a 'Config' by using '<>' to combine any of the following:

* Either 'configKey' or 'configKeyFile' (not both)
* Optionally, 'configSession'

Then use the config as the first argument to the 'runLeanpub' function. -}

newtype Config = Config (ConfigData -> ConfigData)
  deriving (Semigroup, Monoid) via (Endo ConfigData)

data ConfigData =
  ConfigData
    { configData_session :: Maybe Session
    , configData_key :: KeyConfig
    }

data KeyConfig
  = KeyConfig_Pure ApiSecretKey
  | KeyConfig_File FilePath
  | KeyConfig_Nothing

baseConfigData :: ConfigData
baseConfigData =
  ConfigData
    { configData_session = Nothing
    , configData_key = KeyConfig_Nothing
    }

readKeyFile :: FilePath -> IO ApiSecretKey
readKeyFile fp =
    (ApiSecretKey . Data.Text.strip) <$> Data.Text.IO.readFile fp

configSession :: Session -> Config
configSession x = Config (\c -> c { configData_session = Just x })

configKey :: ApiSecretKey -> Config
configKey x = Config (\c -> c { configData_key = KeyConfig_Pure x })

configKeyFile :: FilePath -> Config
configKeyFile x = Config (\c -> c { configData_key = KeyConfig_File x })

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

wreqPostAeson_ :: Path -> [FormParam] -> Leanpub ()
wreqPostAeson_ path params = void (wreqPostAeson path params)

------------------------------------------------------------

getBookSummary :: BookSlug -> Leanpub Data.Aeson.Value
getBookSummary (BookSlug slug) =
    wreqGetAeson [slug] []

getBookSalesSummary :: BookSlug -> Leanpub Data.Aeson.Value
getBookSalesSummary (BookSlug slug) =
  do
    requireKey
    wreqGetAeson [slug, text "sales"] []

createFreeBookCoupon
    :: BookSlug           -- ^ What book does the coupon give away?
    -> CouponCode         -- ^ The secret that the user needs to have
                          --   to redeem the coupon
    -> CouponMaxUses      -- ^ How many times can each coupon be used?
    -> Maybe CouponNote   -- ^ An optional note to remind you what the
                          --   coupon is for, why it was issued, etc.
    -> Leanpub Data.Aeson.Value
createFreeBookCoupon (BookSlug slug) code uses noteMaybe =
  do
    requireKey
    start <- liftIO getToday

    wreqPostAeson
        [slug, text "coupons"]
        (freeBookParams start code uses noteMaybe)

createManyFreeBookCoupons
    :: (CouponCode -> IO ())  -- ^ Action to perform after creating each coupon,
                              --   e.g. perhaps 'print' for use in a REPL.
    -> Natural                -- ^ How many coupons?
    -> BookSlug               -- ^ What book does the coupon give away?
    -> CouponMaxUses          -- ^ How many times can each coupon be used?
    -> Maybe CouponNote       -- ^ An optional note to remind you what the
                              --   coupon is for, why it was issued, etc.
    -> Leanpub ()
createManyFreeBookCoupons done n (BookSlug slug) uses noteMaybe =
  do
    requireKey
    start <- liftIO getToday

    sequence_ $ replicate (fromIntegral n) $
      do
        code <- liftIO randomCouponCode
        wreqPostAeson_
            [slug, text "coupons"]
            (freeBookParams start code uses noteMaybe)
        liftIO (done code)

freeBookParams
    :: Day -> CouponCode -> CouponMaxUses -> Maybe CouponNote
    -> [FormParam]
freeBookParams start (CouponCode code) uses noteMaybe =
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
        , case noteMaybe of
            Nothing -> Nothing
            Just (CouponNote note) -> Just (ascii "coupon[note]" := note)
        ]

getToday :: IO Day
getToday = Data.Time.utctDay <$> Data.Time.getCurrentTime

formatDay :: Day -> String
formatDay = Data.Time.formatTime Data.Time.defaultTimeLocale "%Y-%m-%d"

randomCouponCode :: IO CouponCode
randomCouponCode =
  do
    s <- sequence (Data.List.replicate 20 randomChar)
    return (CouponCode (text s))
  where
    randomChar = pickOne charset
    charset = ['a'..'z'] ++ ['0'..'9']
