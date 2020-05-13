{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Client.Build
  ( defineQuery,
  )
where

--
-- MORPHEUS

import Data.Morpheus.Client.Aeson
  ( deriveFromJSON,
    deriveToJSON,
  )
import Data.Morpheus.Client.Fetch
  ( deriveFetch,
  )
import Data.Morpheus.Client.Transform.Selection
  ( ClientDefinition (..),
    toClientDefinition,
  )
import Data.Morpheus.Core
  ( validateRequest,
  )
import Data.Morpheus.Error
  ( gqlWarnings,
    renderGQLErrors,
  )
import Data.Morpheus.Internal.TH
  ( Scope (..),
    declareType,
  )
import qualified Data.Morpheus.Types.Internal.AST as O
  ( Operation (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( DataTypeKind (..),
    GQLQuery (..),
    Schema,
    TypeD (..),
    TypeD (..),
    VALIDATION_MODE (..),
    isOutputObject,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Result (..),
  )
import Data.Semigroup ((<>))
import Data.Text (unpack)
import Language.Haskell.TH

defineQuery :: IO (Eventless Schema) -> (GQLQuery, String) -> Q [Dec]
defineQuery ioSchema (query, src) = do
  schema <- runIO ioSchema
  case schema >>= (`validateWith` query) of
    Failure errors -> fail (renderGQLErrors errors)
    Success {result, warnings} -> gqlWarnings warnings >> defineQueryD src result

defineQueryD :: String -> ClientDefinition -> Q [Dec]
defineQueryD _ ClientDefinition {clientTypes = []} = return []
defineQueryD src ClientDefinition {clientArguments, clientTypes = rootType : subTypes} =
  do
    rootDecs <-
      defineOperationType
        (queryArgumentType clientArguments)
        src
        rootType
    subTypeDecs <- concat <$> traverse declareT subTypes
    return $ rootDecs ++ subTypeDecs
  where
    declareT clientType@TypeD {tKind}
      | isOutputObject tKind || tKind == KindUnion =
        withToJSON
          declareOutputType
          clientType
      | tKind == KindEnum = withToJSON declareInputType clientType
      | otherwise = declareInputType clientType

declareOutputType :: TypeD -> Q [Dec]
declareOutputType typeD = pure [declareType CLIENT False Nothing [''Show] typeD]

declareInputType :: TypeD -> Q [Dec]
declareInputType typeD = do
  toJSONDec <- deriveToJSON typeD
  pure $ declareType CLIENT True Nothing [''Show] typeD : toJSONDec

withToJSON :: (TypeD -> Q [Dec]) -> TypeD -> Q [Dec]
withToJSON f datatype = do
  toJson <- deriveFromJSON datatype
  dec <- f datatype
  pure (toJson : dec)

queryArgumentType :: Maybe TypeD -> (Type, Q [Dec])
queryArgumentType Nothing = (ConT $ mkName "()", pure [])
queryArgumentType (Just rootType@TypeD {tName}) =
  (ConT $ mkName $ unpack tName, declareInputType rootType)

defineOperationType :: (Type, Q [Dec]) -> String -> TypeD -> Q [Dec]
defineOperationType (argType, argumentTypes) query clientType =
  do
    rootType <- withToJSON declareOutputType clientType
    typeClassFetch <- deriveFetch argType (tName clientType) query
    argsT <- argumentTypes
    pure $ rootType <> typeClassFetch <> argsT

validateWith :: Schema -> GQLQuery -> Eventless ClientDefinition
validateWith schema rawRequest@GQLQuery {operation} = do
  validOperation <- validateRequest schema WITHOUT_VARIABLES rawRequest
  toClientDefinition
    schema
    (O.operationArguments operation)
    validOperation
