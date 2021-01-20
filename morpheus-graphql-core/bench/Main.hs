{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import qualified Data.Morpheus.Core as Morpheus (parseSchema)
import Data.Morpheus.Internal.Ext (resultOr)

main :: IO ()
main = defaultMain [schemaBenchmark "type Query {}"]

parse :: ByteString -> ByteString
parse x = resultOr (error . show) (const "OK") (Morpheus.parseSchema x)

schemaBenchmark :: ByteString -> Benchmark
schemaBenchmark text =
  bgroup
    "basic"
    [ bench "morpheus" $ nf parse text,
      bench "graphql" $ nf parse text,
      bench "hasura" $ nf parse text
    ]
