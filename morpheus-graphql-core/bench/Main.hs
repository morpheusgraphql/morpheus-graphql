{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Criterion.Types
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

data Info = Info
  { lineCount :: Int,
    name :: String,
    src :: FilePath
  }

getFile :: Info -> IO (Info, ByteString, Text)
getFile info = do
  (b, t) <- fetchCase (src info)
  pure (info, b, t)

getFiles :: IO [(Info, ByteString, Text)]
getFiles =
  traverse
    getFile
    [ Info {lineCount = 94, name = "mythology", src = "mythology"},
      Info {lineCount = 2500, name = "descriptions", src = "descriptions"},
      --      Info {lineCount = 38948, name = "github", src = "github"},
      Info {lineCount = 5922, name = "starWars", src = "starWars"}
      --    Info {lineCount = 6, name = "wrappers", src = "wrappers"}
    ]

typeCount :: (ByteString, Text) -> String
typeCount (bs, txt) =
  "morpheus("
    <> show (Morpheus.countParsedTypes bs)
    <> ") - gql("
    <> show (GQL.countParsedTypes txt)
    <> ")"

benchText :: (Text -> Text) -> [(Info, ByteString, Text)] -> [Benchmark]
benchText f = map (\(Info {name}, _, t) -> bench name $ whnf f t)

benchByteString :: (ByteString -> ByteString) -> [(Info, ByteString, Text)] -> [Benchmark]
benchByteString f = map (\(Info {name}, b, _) -> bench name $ whnf f b)

gqlBenchmark :: [(Info, ByteString, Text)] -> Benchmark
gqlBenchmark cases =
  bgroup
    "GraphQL"
    [ bgroup "Text" $ benchText GQL.parseText cases
    ]

morpheusBenchmark :: [(Info, ByteString, Text)] -> Benchmark
morpheusBenchmark cases =
  bgroup
    "Morpheus"
    [ bgroup "Text" $ benchText Morpheus.parseText cases,
      bgroup "ByteString" $ benchByteString Morpheus.parseByteString cases
    ]

main :: IO ()
main = do
  files <- getFiles
  defaultMainWith
    ( defaultConfig
        { reportFile = Just "report.html",
          resamples = 100000,
          timeLimit = 60
        }
    )
    [ gqlBenchmark files,
      morpheusBenchmark files
    ]
