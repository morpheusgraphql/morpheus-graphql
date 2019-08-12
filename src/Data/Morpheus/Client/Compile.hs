{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Client.Compile
  ( compile
  ) where

import           Data.Aeson                           (encode)
import           Data.ByteString.Lazy.Char8           (ByteString, unpack)
import qualified Data.Text                            as T (pack)
import           Language.Haskell.TH

--
--  Morpheus
import           Data.Morpheus.Client.Data            (QueryD (..))
import           Data.Morpheus.Client.Selection       (operationTypes)
import           Data.Morpheus.Document.ParseDocument (parseFullGQLDocument)
import           Data.Morpheus.Error.Utils            (renderErrors)
import           Data.Morpheus.Parser.Parser          (parseGQL)
import           Data.Morpheus.Types.IO               (GQLRequest (..))
import           Data.Morpheus.Validation.Validation  (validateRequest)

compile :: IO ByteString -> String -> Q Exp
compile ioSchema queryText = do
  eitherSchema <- parseFullGQLDocument <$> runIO ioSchema
  case eitherSchema of
    Left errors -> fail (show errors)
    Right schema ->
      case parseGQL request >>= validateRequest schema of
        Left errors -> fail gqlCompileErrors
          where gqlCompileErrors = unpack $ encode $ renderErrors errors
        Right operation ->
          case operationTypes schema operation of
            Left err -> fail $ show err
            Right queryTypes -> [|queryD|]
              where queryD = QueryD {queryText, queryTypes, queryArgTypes}
                    queryArgTypes = []
  where
    request = GQLRequest {query = T.pack queryText, operationName = Nothing, variables = Nothing}
