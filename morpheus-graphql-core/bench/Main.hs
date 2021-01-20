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

fetchCase :: FilePath -> IO (ByteString, Text)
fetchCase x = (,) <$> L.readFile path <*> TIO.readFile path
  where
    path = "bench/assets/" <> x <> ".gql"

main :: IO ()
main = do
  github <- fetchCase "github"
  mythology <- fetchCase "mythology"
  starwars <- fetchCase "starwars"
  defaultMain
    [ schemaBenchmark "github: 38,948 lines" github,
      schemaBenchmark "mythology: 94 lines" mythology,
      schemaBenchmark "starwars: 5,922 lines" starwars
    ]

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

schemaBenchmark :: String -> (ByteString, Text) -> Benchmark
schemaBenchmark label (bs, txt) =
  bgroup
    label
    [ bench "morpheus" $ nf parseMorpheus bs,
      bench "graphql" $ nf parseGraphQL txt
    ]
