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
    m',
    mkFieldName,
    mkTypeName,
    nameConType,
    nameConType,
    nameSpaceField,
    nameSpaceType,
    tyConArgs,
  )
import qualified Data.Morpheus.Types.Internal.AST as O
  ( Operation (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition (..),
    ConsD (..),
    FieldDefinition (..),
    GQLQuery (..),
    Schema,
    TypeKind (..),
    VALIDATION_MODE (..),
    isOutputObject,
    isSubscription,
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

mkTHTypeName :: TypeNameTH -> Name
mkTHTypeName TypeNameTH {namespace, typename} =
  mkTypeName $
    nameSpaceType namespace typename

declareType ::
  Bool ->
  Maybe TypeKind ->
  [Name] ->
  ClientTypeDefinition ->
  Dec
declareType
  namespaceOpt
  kindD
  derivingList
  ClientTypeDefinition
    { clientTypeName = TypeNameTH {namespace, typename},
      clientCons
    } =
    DataD
      []
      (genName typename)
      tVars
      Nothing
      cons
      (map derive (''Generic : derivingList))
    where
      genName = mkTypeName . nameSpaceType namespace
      tVars = maybe [] (declareTyVar . tyConArgs) kindD
        where
          declareTyVar = map (PlainTV . mkTypeName)
      defBang = Bang NoSourceUnpackedness NoSourceStrictness
      derive className = DerivClause Nothing [ConT className]
      cons
        | isEnum clientCons = map consE clientCons
        | otherwise = map consR clientCons
      consE ConsD {cName} = NormalC (genName $ typename <> cName) []
      consR ConsD {cName, cFields} =
        RecC
          (genName cName)
          (map declareField cFields)
        where
          declareField FieldDefinition {fieldName, fieldContent = fieldArgs, fieldType} =
            (mkFieldName fName, defBang, fiType)
            where
              fName
                | namespaceOpt = nameSpaceField typename fieldName
                | otherwise = fieldName
              fiType = genFieldT fieldArgs
                where
                  ---------------------------
                  genFieldT _
                    | (isOutputObject <$> kindD) == Just True = AppT m' result
                    | otherwise = result
                  ------------------------------------------------
                  result = declareTypeRef (maybe False isSubscription kindD) fieldType

declareOutputType :: ClientTypeDefinition -> Q [Dec]
declareOutputType typeD = pure [declareType False Nothing [''Show] typeD]

declareInputType :: ClientTypeDefinition -> Q [Dec]
declareInputType typeD = do
  toJSONDec <- deriveToJSON typeD
  pure $ declareType True Nothing [''Show] typeD : toJSONDec

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
