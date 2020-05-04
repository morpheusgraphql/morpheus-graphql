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
import Data.Morpheus.Client.Compile
  ( validateWith,
  )
import Data.Morpheus.Client.Error
  ( gqlWarnings,
    renderGQLErrors,
  )
import Data.Morpheus.Client.Fetch
  ( deriveFetch,
  )
import Data.Morpheus.Execution.Internal.Declare
  ( Scope (..),
    declareType,
  )
import Data.Morpheus.Types.Internal.AST
  ( ClientQuery (..),
    ClientType (..),
    DataTypeKind (..),
    GQLQuery (..),
    Schema,
    TypeD (..),
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
defineQuery ioSchema queryRoot = do
  schema <- runIO ioSchema
  case schema >>= (`validateWith` queryRoot) of
    Failure errors -> fail (renderGQLErrors errors)
    Success {result, warnings} -> gqlWarnings warnings >> defineQueryD result

defineQueryD :: ClientQuery -> Q [Dec]
defineQueryD ClientQuery {queryTypes = rootType : subTypes, queryText, queryArgsType} =
  do
    rootDecs <-
      defineOperationType
        (queryArgumentType queryArgsType)
        queryText
        rootType
    subTypeDecs <- concat <$> traverse declareT subTypes
    return $ rootDecs ++ subTypeDecs
  where
    declareT ClientType {clientType, clientKind}
      | isOutputObject clientKind || clientKind == KindUnion =
        withToJSON
          declareOutputType
          clientType
      | clientKind == KindEnum = withToJSON declareInputType clientType
      | otherwise = declareInputType clientType
defineQueryD ClientQuery {queryTypes = []} = return []

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

defineOperationType :: (Type, Q [Dec]) -> String -> ClientType -> Q [Dec]
defineOperationType (argType, argumentTypes) query ClientType {clientType} =
  do
    rootType <- withToJSON declareOutputType clientType
    typeClassFetch <- deriveFetch argType (tName clientType) query
    argsT <- argumentTypes
    pure $ rootType <> typeClassFetch <> argsT
