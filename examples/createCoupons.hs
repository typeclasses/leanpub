import Leanpub.Concepts
import Leanpub.Wreq

import qualified Data.Text
import qualified Data.Text.IO

-- Read the API key from a file
config = configKeyFile "/home/chris/.config/typeclasses/leanpub-api-key.txt"

-- Which book we're creating coupons for
slug = BookSlug (Data.Text.pack "finding-success-in-haskell")

-- A note reminding us why the coupon was issued
note = CouponNote (Data.Text.pack "Free for Type Classes subscriber")

-- Each coupon can be used at most twice
uses = CouponMaxUses 2

-- After creating each coupon, append it to a file
save (CouponCode x) = Data.Text.IO.appendFile
  "/home/chris/typeclasses/coupons.txt" (x <> Data.Text.pack "\n")

main = runLeanpub config
    (createManyFreeBookCoupons save 200 slug uses (Just note))
