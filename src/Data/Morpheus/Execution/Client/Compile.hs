{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Execution.Client.Compile
  ( compileWith
  ) where

import           Data.Aeson                                 (encode)
import           Data.ByteString.Lazy.Char8                 (unpack)
import qualified Data.Text                                  as T (pack)
import           Language.Haskell.TH

--
--  Morpheus
import           Data.Morpheus.Error.Utils                  (renderErrors)
import           Data.Morpheus.Execution.Client.Data        (QueryD (..))
import           Data.Morpheus.Execution.Client.Selection   (operationTypes)
import           Data.Morpheus.Parsing.Request.Parser       (parseGQL)
import qualified Data.Morpheus.Types.Internal.AST.Operation as O (Operation (..))
import           Data.Morpheus.Types.Internal.Data          (DataTypeLib)
import           Data.Morpheus.Types.Internal.Validation    (Validation)
import           Data.Morpheus.Types.IO                     (GQLRequest (..))
import           Data.Morpheus.Types.Types                  (GQLQueryRoot (..))
import           Data.Morpheus.Validation.Utils.Utils       (VALIDATION_MODE (..))
import           Data.Morpheus.Validation.Validation        (validateRequest)

compileWith :: IO (Validation DataTypeLib) -> String -> Q Exp
compileWith ioSchema queryText = do
  mSchema <- runIO ioSchema
  case validateBy mSchema of
    Left errors  -> fail (unpack $ encode $ renderErrors errors)
    Right queryD -> [|queryD|]
  where
    validateBy mSchema = do
      schema <- mSchema
      rawRequest@GQLQueryRoot {operation} <- parseGQL request
      validOperation <- validateRequest schema WITHOUT_VARIABLES rawRequest
      (queryArgTypes, queryTypes) <- operationTypes schema (O.operationArgs operation) validOperation
      return QueryD {queryText, queryTypes, queryArgTypes}
    request = GQLRequest {query = T.pack queryText, operationName = Nothing, variables = Nothing}
