{-# OPTIONS_GHC -Wall #-}

module Leanpub.Concepts
  ( ApiSecretKey (..)
  , BookSlug (..)
  , CouponCode (..)
  , CouponMaxUses (..)
  ) where

-- base
import Numeric.Natural (Natural)

-- text
import Data.Text (Text)

{- | Get an API key from the
<https://leanpub.com/author_dashboard/settings Leanpub dashboard>.
This API key should be kept private; treat it just like your password
to your Leanpub account. -}

newtype ApiSecretKey = ApiSecretKey Text

{- | An identifier for a book. E.g. if your book is found at

> https://leanpub.com/your_book

then your book's slug is @your_book@. -}

newtype BookSlug = BookSlug Text

{- | An identifier for a coupon. E.g. if your book's slug is @your_book@ and
the coupon code is @black_friday@ then users can use your coupon via the URL:

> https://leanpub.com/your_book/c/black_friday

-}

newtype CouponCode = CouponCode Text

data CouponMaxUses
  = CouponUseUnlimited
      -- ^ There is no limit to how many times the coupon may be used.
  | CouponMaxUses Natural
      -- ^ The maximum number of times the coupon may be used.
