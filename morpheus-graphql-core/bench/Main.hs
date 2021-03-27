{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import qualified Data.ByteString.Lazy as L (readFile)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Foldable (toList)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Parser.GraphQL as GQL
import Parser.Morpheus as Morpheus

fetchCase :: FilePath -> IO (ByteString, Text)
fetchCase x = (,) <$> L.readFile path <*> TIO.readFile path
  where
    path = "bench/assets/" <> x <> ".gql"

fetch :: FilePath -> IO (String, (ByteString, Text))
fetch path = do
  x <- fetchCase path
  pure (typeCount x, x)

main :: IO ()
main = do
  github <- fetch "github"
  mythology <- fetch "mythology"
  starWars <- fetch "starwars"
  wrappers <- fetch "wrappers"
  descriptions <- fetch "descriptions"
  defaultMain
    [ schemaBenchmark "github: \nlines: 38,948 " github,
      schemaBenchmark "mythology: \nlines: 94 " mythology,
      schemaBenchmark "starWars: \nlines: 5,922 " starWars,
      schemaBenchmark "wrappers: \nlines: 6 " wrappers,
      schemaBenchmark "descriptions: \nlines: 2500 " descriptions
    ]

typeCount :: (ByteString, Text) -> String
typeCount (bs, txt) = "morpheus(" <> show morpheus <> ") - gql(" <> show gql <> ")"
  where
    morpheus = Morpheus.countParsedTypes bs
    gql = GQL.countParsedTypes txt

schemaBenchmark :: String -> (String, (ByteString, Text)) -> Benchmark
schemaBenchmark label (count, (bs, txt)) =
  bgroup
    (label <> "\n type number: " <> count <> "\n library: ")
    [ bench "graphql parsing Text" $ nf GQL.parseText txt,
      bench "morpheus parsing ByteString" $ nf Morpheus.parseByteString bs,
      bench "morpheus parsing Text" $ nf Morpheus.parseText txt
    ]
