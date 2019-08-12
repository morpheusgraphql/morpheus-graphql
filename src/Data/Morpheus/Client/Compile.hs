{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Client.Compile
  ( compile
  ) where

import           Data.Aeson                                (encode)
import           Data.ByteString.Lazy.Char8                (ByteString, unpack)
import qualified Data.Text                                 as T (pack)
import           Language.Haskell.TH

--
--  Morpheus
import           Data.Morpheus.Client.Data                 (QueryD (..))
import           Data.Morpheus.Client.Selection            (operationTypes)
import           Data.Morpheus.Document.ParseDocument      (parseFullGQLDocument)
import           Data.Morpheus.Error.Utils                 (renderErrors)
import           Data.Morpheus.Parser.Parser               (parseGQL)
import           Data.Morpheus.Types.Internal.AST.Operator (Operator' (..), unpackOperator)
import           Data.Morpheus.Types.IO                    (GQLRequest (..))
import           Data.Morpheus.Types.Types                 (GQLQueryRoot (..))
import           Data.Morpheus.Validation.Validation       (validateRequest)

compile :: IO ByteString -> String -> Q Exp
compile ioSchema queryText = do
  eitherSchema <- parseFullGQLDocument <$> runIO ioSchema
  case eitherSchema of
    Left errors -> fail (show errors)
    Right schema ->
      case parseGQL request of
        Left compErrors -> fail (show compErrors)
        Right rawOperator ->
          case validateRequest schema rawOperator of
            Left errors -> fail (unpack $ encode $ renderErrors errors)
            Right validOperation ->
              case operationTypes schema (operatorArgs $ unpackOperator $ operator rawOperator) validOperation of
                Left err -> fail $ show err
                Right (queryArgTypes, queryTypes) -> [|queryD|]
                  where queryD = QueryD {queryText, queryTypes, queryArgTypes}
  where
    request = GQLRequest {query = T.pack queryText, operationName = Nothing, variables = Nothing}
