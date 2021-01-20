{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.Morpheus.Core (parseSchema)
import Data.Morpheus.Internal.Ext (resultOr)

main :: IO ()
main = defaultMain [decodeBenchmarks]

parse :: ByteString -> ByteString
parse x = resultOr (pack . show) (const "OK") (parseSchema x)

decodeBenchmarks :: Benchmark
decodeBenchmarks =
  bgroup
    "direct"
    [ bench "morpheus" $ nf parse "{}",
      bench "graphql" $ nf parse "{}"
    ]
