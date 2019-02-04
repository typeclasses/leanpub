{-# OPTIONS_GHC -Wall #-}

module Leanpub.Concepts
  (
  -- * API
    ApiSecretKey (..)

  -- * Books
  , BookSlug (..)
  , bookURL

  -- * Coupons
  , CouponCode (..)
  , couponURL
  , CouponMaxUses (..)
  , CouponNote (..)

  ) where

-- base
import Numeric.Natural (Natural)

-- text
import Data.Text (Text)
import qualified Data.Text.Lazy         as LT
import qualified Data.Text.Lazy.Builder as TB

{- | Get an API key from the
<https://leanpub.com/author_dashboard/settings Leanpub dashboard>.
This API key should be kept private; treat it just like your password
to your Leanpub account. -}

newtype ApiSecretKey = ApiSecretKey Text

{- | An identifier for a book. E.g. if your book is found at

> https://leanpub.com/your_book

then your book's slug is @your_book@. -}

newtype BookSlug = BookSlug Text
    deriving Show

{- |
>>> :set -XOverloadedStrings
>>> bookURL (BookSlug "your_book")
"https://leanpub.com/your_book"
-}

bookURL :: BookSlug -> Text
bookURL (BookSlug book) =
    (LT.toStrict . TB.toLazyText)
        (  TB.fromString  "https://leanpub.com/"
        <> TB.fromText    book
        )

{- | An identifier for a coupon. -}

newtype CouponCode = CouponCode Text
    deriving Show

{- | E.g. if your book's slug is @your_book@ and the coupon code is
@black_friday@ then users can use your coupon via the URL:

> https://leanpub.com/your_book/c/black_friday

(Don't just give the code at the end of the coupon to your potential customers,
since there's nowhere to type it in on the checkout form.)

>>> :set -XOverloadedStrings
>>> couponURL (BookSlug "your_book") (CouponCode "black_friday")
"https://leanpub.com/your_book/c/black_friday"
-}

couponURL :: BookSlug -> CouponCode -> Text
couponURL (BookSlug book) (CouponCode coupon) =
    (LT.toStrict . TB.toLazyText)
        (  TB.fromString  "https://leanpub.com/"
        <> TB.fromText    book
        <> TB.fromString  "/c/"
        <> TB.fromText    coupon
        )

data CouponMaxUses
    = CouponUseUnlimited
        -- ^ There is no limit to how many times the coupon may be used.
    | CouponMaxUses Natural
        -- ^ The maximum number of times the coupon may be used.
    deriving Show

{- | A description of a coupon. This is just used to remind you of what it was
for; it is not visible to users. -}

data CouponNote = CouponNote Text
    deriving Show
