{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Criterion.Main
import qualified Data.ByteString.Lazy as L (readFile)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Foldable (toList)
import qualified Data.Morpheus.Core as Morpheus
import Data.Morpheus.Internal.Ext (resultOr)
import Data.String
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Language.GraphQL.AST as GQL
import Text.Megaparsec (runParser)

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
  print (parseGraphQL (snd $ snd github))
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
    morpheus = resultOr (const 0) length (Morpheus.parseTypeDefinitions bs)
    gql = either (const 0) length (parseTypeSysDefinition txt)

parseMorpheus :: ByteString -> ByteString
parseMorpheus x = resultOr (error . show) (const "OK") (Morpheus.parseTypeDefinitions x)

parseGraphQL :: Text -> Text
parseGraphQL x = either (error . show) (T.pack . show) (parseTypeSysDefinition x)

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

schemaBenchmark :: String -> (String, (ByteString, Text)) -> Benchmark
schemaBenchmark label (count, (bs, txt)) =
  bgroup
    (label <> "\n type number: " <> count <> "\n library: ")
    [ bench "graphql" $ nf parseGraphQL txt,
      bench "morpheus" $ nf parseMorpheus bs
    ]
