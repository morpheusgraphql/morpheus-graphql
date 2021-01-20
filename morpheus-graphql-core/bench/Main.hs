{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Criterion.Main
import qualified Data.ByteString.Lazy as L (readFile)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Foldable (toList)
import qualified Data.Morpheus.Core as Morpheus
import Data.Morpheus.Internal.Ext (resultOr)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Language.GraphQL.AST as GQL
import Text.Megaparsec (runParser)

main :: IO ()
main = do
  fileBS <- L.readFile "bench/schema.gql"
  fileTS <- TIO.readFile "bench/schema.gql"
  defaultMain [schemaBenchmark fileBS fileTS]

parseMorpheus :: ByteString -> ByteString
parseMorpheus x = resultOr (error . show) (const "OK") (Morpheus.parseTypeDefinitions x)

parseGraphQL :: Text -> Text
parseGraphQL x = either (error . show) (const "OK") (parseTypeSysDefinition x)

parseDoc :: T.Text -> Either T.Text [GQL.Definition]
parseDoc s =
  case runParser GQL.document "<doc>" s of
    Right d -> Right (toList d)
    Left e -> Left (T.pack $ show e)

parseTypeSysDefinition :: Text -> Either Text [GQL.TypeSystemDefinition]
parseTypeSysDefinition s =
  case runParser GQL.document "<doc>" s of
    Right (toList -> d) ->
      let tds = [td | GQL.TypeSystemDefinition td _ <- d]
       in if length d == length tds
            then Right tds
            else Left "unexpected query or type system extension"
    Left e ->
      Left (T.pack $ show e)

schemaBenchmark :: ByteString -> Text -> Benchmark
schemaBenchmark bs txt =
  bgroup
    "basic"
    [ bench "morpheus" $ nf parseMorpheus bs,
      bench "graphql" $ nf parseGraphQL txt
    ]
