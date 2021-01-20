{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.Morpheus.Core (parseSchema)
import Data.Morpheus.Internal.Ext (resultOr)

main :: IO ()
main = defaultMain [schemaBenchmark "type Query {}"]

parse :: ByteString -> ByteString
parse x = resultOr (error . show) (const "OK") (parseSchema x)

schemaBenchmark :: ByteString -> Benchmark
schemaBenchmark text =
  bgroup
    "basic"
    [ bench "morpheus" $ nf parse text,
      bench "graphql" $ nf parse text,
      bench "hasura" $ nf parse text
    ]
