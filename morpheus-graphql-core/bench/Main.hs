{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Criterion.Main
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.Foldable (toList)
import qualified Data.Morpheus.Core as Morpheus (parseTypeDefinitions)
import Data.Morpheus.Internal.Ext (resultOr)
import Data.Morpheus.Internal.Utils (fromLBS)
import qualified Data.Text as T
import Language.GraphQL.AST (document)
import qualified Language.GraphQL.AST as GQL
import qualified Language.GraphQL.AST.Parser as GraphQL (document)
import Text.Megaparsec (runParser)

main :: IO ()
main = defaultMain [schemaBenchmark "type Query {}"]

parseMorpheus :: ByteString -> ByteString
parseMorpheus x = resultOr (error . show) (const "OK") (Morpheus.parseTypeDefinitions x)

parseGraphQL :: ByteString -> ByteString
parseGraphQL x = either (error . show) (const "OK") (parseTypeSysDefinition x)

parseDoc :: T.Text -> Either T.Text [GQL.Definition]
parseDoc s =
  case runParser document "<doc>" s of
    Right d -> Right (toList d)
    Left e -> Left (T.pack $ show e)

parseTypeSysDefinition :: ByteString -> Either T.Text [GQL.TypeSystemDefinition]
parseTypeSysDefinition s =
  case runParser document "<doc>" (fromLBS s) of
    Right (toList -> d) ->
      let tds = [td | GQL.TypeSystemDefinition td _ <- d]
       in if length d == length tds
            then Right tds
            else Left "unexpected query or type system extension"
    Left e ->
      Left (T.pack $ show e)

schemaBenchmark :: ByteString -> Benchmark
schemaBenchmark text =
  bgroup
    "basic"
    [ bench "morpheus" $ nf parseMorpheus text,
      bench "graphql" $ nf parseGraphQL text
    ]
