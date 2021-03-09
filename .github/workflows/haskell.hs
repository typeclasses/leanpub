import System.Environment
import System.Process

main =
  getEnv "ghc" >>= \ghc ->
    case ghc of
      "8.8.3"  -> callProcess "cabal" ["build", "all"
                  , "--constraint=aeson == 1.4.*"
                  , "--constraint=bytestring == 0.10.*"
                  , "--constraint=text == 1.2.4.0"
                  , "--constraint=time == 1.9.*"
                  ]
      "8.10.3" -> callProcess "cabal" ["build", "all"
                  , "--constraint=aeson == 1.5.*"
                  , "--constraint=bytestring == 0.11.*"
                  , "--constraint=time == 1.11.*"
                  ]
