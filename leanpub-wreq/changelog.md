1.0.0.0 - 2019 Feb 3

  * Initial release

1.1 - 2021 Mar 5

  * Drop support for GHC 8.6
  * Add support for GHC 8.8 and 8.10
  * Tighten dependency bounds

1.1.0.1 - 2021 Mar 9

  * Change dependency from `rando` to `mwc-random`
    for generating random coupon codes

  * Relax dependency bounds

  * Remove `lens` dependency (although `wreq`
    incurs it transitively anyway)

1.1.0.2 - 2021 Mar 10

  * Relax mwc-random lower bound to allow 0.14
