{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Morpheus.Client
  ( gql
  , defineQuery
  , Fetch(..)
  , TypeD(..)
  , FieldD(..)
  , ConsD(..)
  ) where

--import           Data.Data
import           Data.Aeson                           (encode)
import qualified Data.ByteString.Lazy                 as L (readFile)
import           Data.ByteString.Lazy.Char8           (ByteString, unpack)
import qualified Data.Text                            as T (pack)

--- Template Haskell
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Data.Morpheus.Client.Build           (Fetch (..), defineQuery)

--
--  Morpheus
import           Data.Morpheus.Client.Data            (ConsD (..), FieldD (..), TypeD (..))
import           Data.Morpheus.Client.Selection       (operationTypes)
import           Data.Morpheus.Document.ParseDocument (parseFullGQLDocument)
import           Data.Morpheus.Error.Utils            (renderErrors)
import           Data.Morpheus.Parser.Parser          (parseGQL)
import           Data.Morpheus.Types.IO               (GQLRequest (..))
import           Data.Morpheus.Validation.Validation  (validateRequest)

gql :: QuasiQuoter
gql =
  QuasiQuoter
    { quoteExp = compile
    , quotePat = notHandled "patterns"
    , quoteType = notHandled "types"
    , quoteDec = notHandled "declarations"
    }
  where
    notHandled things = error $ things ++ " are not handled by the regex quasiquoter."

readSchema :: IO ByteString
readSchema = L.readFile "./assets/simple.gql"

compile :: String -> Q Exp
compile queryText = do
  eitherSchema <- parseFullGQLDocument <$> runIO readSchema
  case eitherSchema of
    Left errors -> fail (show errors)
    Right schema ->
      case parseGQL request >>= validateRequest schema of
        Left errors -> fail gqlCompileErrors
          where gqlCompileErrors = unpack $ encode $ renderErrors errors
        Right operation -> [|buildCon|]
          where buildCon :: ([TypeD], String)
                buildCon =
                  case operationTypes schema operation of
                    Left err -> fail $ show err
                    Right x  -> (x, queryText)
  where
    request = GQLRequest {query = T.pack queryText, operationName = Nothing, variables = Nothing}
