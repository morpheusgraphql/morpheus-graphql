{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Data.Morpheus.Execution.Client.Build
  ( defineQuery
  )
where

import           Data.Semigroup                 ( (<>) )
import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Error.Client.Client
                                                ( renderGQLErrors )
import           Data.Morpheus.Execution.Client.Aeson
                                                ( deriveFromJSON
                                                , deriveToJSON
                                                )
import           Data.Morpheus.Execution.Client.Compile
                                                ( validateWith )
import           Data.Morpheus.Execution.Client.Fetch
                                                ( deriveFetch )
import           Data.Morpheus.Execution.Internal.Declare
                                                ( declareType )
import           Data.Morpheus.Execution.Internal.Utils
                                                ( gqlWarnings )
import           Data.Morpheus.Types.Internal.AST.Data
                                                ( DataTypeKind(..)
                                                , DataTypeLib
                                                , isOutputObject
                                                , ClientType(..)
                                                , ClientQuery(..)
                                                , TypeD(..)
                                                )
import           Data.Morpheus.Types.Internal.Validation
                                                ( Validation
                                                , Computation(..)
                                                )
import           Data.Morpheus.Types.Types      ( GQLQueryRoot(..) )



defineQuery :: IO (Validation DataTypeLib) -> (GQLQueryRoot, String) -> Q [Dec]
defineQuery ioSchema queryRoot = do
  schema <- runIO ioSchema
  case schema >>= (`validateWith` queryRoot) of
    Failure errors         -> fail (renderGQLErrors errors)
    Success query warnings -> gqlWarnings warnings >> defineQueryD query


defineQueryD :: ClientQuery -> Q [Dec]
defineQueryD ClientQuery { queryTypes = rootType : subTypes, queryText, queryArgsType }
  = do
    rootDecs <- defineOperationType (queryArgumentType queryArgsType)
                                    queryText
                                    rootType
    subTypeDecs <- concat <$> traverse declareT subTypes
    return $ rootDecs ++ subTypeDecs
 where
  declareT ClientType { clientType, clientKind }
    | isOutputObject clientKind || clientKind == KindUnion = withToJSON
      declareOutputType
      clientType
    | clientKind == KindEnum = withToJSON declareInputType clientType
    | otherwise = declareInputType clientType
defineQueryD ClientQuery { queryTypes = [] } = return []

declareOutputType :: TypeD -> Q [Dec]
declareOutputType typeD = pure [declareType False Nothing [''Show] typeD]

declareInputType :: TypeD -> Q [Dec]
declareInputType typeD = do
  toJSONDec <- deriveToJSON typeD
  pure $ declareType True Nothing [''Show] typeD : toJSONDec

withToJSON :: (TypeD -> Q [Dec]) -> TypeD -> Q [Dec]
withToJSON f datatype = do
  toJson <- deriveFromJSON datatype
  dec    <- f datatype
  pure (toJson : dec)

queryArgumentType :: Maybe TypeD -> (Type, Q [Dec])
queryArgumentType Nothing = (ConT $ mkName "()", pure [])
queryArgumentType (Just rootType@TypeD { tName }) =
  (ConT $ mkName tName, declareInputType rootType)

defineOperationType :: (Type, Q [Dec]) -> String -> ClientType -> Q [Dec]
defineOperationType (argType, argumentTypes) query ClientType { clientType } =
  do
    rootType       <- withToJSON declareOutputType clientType
    typeClassFetch <- deriveFetch argType (tName clientType) query
    argsT          <- argumentTypes
    pure $ rootType <> typeClassFetch <> argsT
