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
    deriveScalarJSON,
    deriveToJSON,
  )
import Data.Morpheus.Client.Fetch
  ( deriveFetch,
  )
import Data.Morpheus.Client.Internal.Types
  ( ClientDefinition (..),
    ClientTypeDefinition (..),
    TypeNameTH (..),
  )
import Data.Morpheus.Client.Transform.Selection
  ( toClientDefinition,
  )
import Data.Morpheus.Core
  ( validateRequest,
  )
import Data.Morpheus.Error
  ( gqlWarnings,
    renderGQLErrors,
  )
import Data.Morpheus.Internal.TH
  ( declareTypeRef,
    isEnum,
    mkFieldName,
    mkTypeName,
    nameConType,
    nameConType,
    nameSpaceType,
  )
import qualified Data.Morpheus.Types.Internal.AST as O
  ( Operation (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( ANY,
    ConsD (..),
    FieldDefinition (..),
    FieldName,
    GQLQuery (..),
    Schema,
    TypeKind (..),
    TypeName,
    VALIDATION_MODE (..),
    isOutputObject,
  )
import Data.Morpheus.Types.Internal.Resolving
  ( Eventless,
    Result (..),
  )
import Data.Semigroup ((<>))
import GHC.Generics (Generic)
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
    rootDeclaration <-
      defineOperationType
        (queryArgumentType clientArguments)
        src
        rootType
    typeDeclarations <- concat <$> traverse declareT subTypes
    pure (rootDeclaration <> typeDeclarations)
  where
    declareT clientType@ClientTypeDefinition {clientKind}
      | isOutputObject clientKind || clientKind == KindUnion =
        withToJSON
          declareOutputType
          clientType
      | clientKind == KindEnum = withToJSON declareInputType clientType
      | clientKind == KindScalar = deriveScalarJSON clientType
      | otherwise = declareInputType clientType

declareType :: ClientTypeDefinition -> Dec
declareType
  ClientTypeDefinition
    { clientTypeName = thName@TypeNameTH {namespace, typename},
      clientCons
    } =
    DataD
      []
      (mkConName namespace typename)
      []
      Nothing
      (declareCons thName clientCons)
      (map derive [''Generic, ''Show])
    where
      derive className = DerivClause Nothing [ConT className]

mkConName :: [FieldName] -> TypeName -> Name
mkConName namespace = mkTypeName . nameSpaceType namespace

declareCons :: TypeNameTH -> [ConsD ANY] -> [Con]
declareCons TypeNameTH {namespace, typename} clientCons
  | isEnum clientCons = map consE clientCons
  | otherwise = map consR clientCons
  where
    consE ConsD {cName} = NormalC (mkConName namespace (typename <> cName)) []
    consR ConsD {cName, cFields} =
      RecC
        (mkConName namespace cName)
        (map declareField cFields)

declareField :: FieldDefinition ANY -> (Name, Bang, Type)
declareField FieldDefinition {fieldName, fieldType} =
  ( mkFieldName fieldName,
    Bang NoSourceUnpackedness NoSourceStrictness,
    declareTypeRef False fieldType
  )

declareOutputType :: ClientTypeDefinition -> Q [Dec]
declareOutputType typeD = pure [declareType typeD]

declareInputType :: ClientTypeDefinition -> Q [Dec]
declareInputType typeD = do
  toJSONDec <- deriveToJSON typeD
  pure $ declareType typeD : toJSONDec

withToJSON :: (ClientTypeDefinition -> Q [Dec]) -> ClientTypeDefinition -> Q [Dec]
withToJSON f datatype = do
  toJson <- deriveFromJSON datatype
  dec <- f datatype
  pure (toJson : dec)

queryArgumentType :: Maybe ClientTypeDefinition -> (Type, Q [Dec])
queryArgumentType Nothing = (nameConType "()", pure [])
queryArgumentType (Just rootType@ClientTypeDefinition {clientTypeName}) =
  (nameConType (typename clientTypeName), declareInputType rootType)

defineOperationType :: (Type, Q [Dec]) -> String -> ClientTypeDefinition -> Q [Dec]
defineOperationType
  (argType, argumentTypes)
  query
  clientType@ClientTypeDefinition
    { clientTypeName = TypeNameTH {typename}
    } =
    do
      rootType <- withToJSON declareOutputType clientType
      typeClassFetch <- deriveFetch argType typename query
      argsT <- argumentTypes
      pure $ rootType <> typeClassFetch <> argsT

validateWith :: Schema -> GQLQuery -> Eventless ClientDefinition
validateWith schema rawRequest@GQLQuery {operation} = do
  validOperation <- validateRequest schema WITHOUT_VARIABLES rawRequest
  toClientDefinition
    schema
    (O.operationArguments operation)
    validOperation
