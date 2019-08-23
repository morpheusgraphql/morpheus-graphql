{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Data.Morpheus.Execution.Client.Build
  ( defineQuery
  ) where

import           Control.Lens                             (declareLenses)
import           Data.Aeson                               (ToJSON)
import           Data.Semigroup                           ((<>))
import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Error.Client.Client        (renderGQLErrors)
import           Data.Morpheus.Execution.Client.Aeson     (deriveFromJSON)
import           Data.Morpheus.Execution.Client.Compile   (validateWith)
import           Data.Morpheus.Execution.Client.Fetch     (deriveFetch)
import           Data.Morpheus.Execution.Internal.Declare (declareType)
import           Data.Morpheus.Types.Internal.Data        (DataTypeLib)
import           Data.Morpheus.Types.Internal.DataD       (QueryD (..), TypeD (..))
import           Data.Morpheus.Types.Internal.Validation  (Validation)
import           Data.Morpheus.Types.Types                (GQLQueryRoot (..))

queryArgumentType :: [TypeD] -> (Type, Q [Dec])
queryArgumentType [] = (ConT $ mkName "()", pure [])
queryArgumentType (rootType@TypeD {tName}:xs) = (ConT $ mkName tName, types)
  where
    types = pure $ map (declareType [''Show, ''ToJSON]) (rootType : xs)

defineJSONType :: TypeD -> Q [Dec]
defineJSONType datatype = do
  record <- declareLenses (pure [declareType [''Show] datatype])
  toJson <- pure <$> deriveFromJSON datatype
  pure $ record <> toJson

defineOperationType :: (Type, Q [Dec]) -> String -> TypeD -> Q [Dec]
defineOperationType (argType, argumentTypes) query datatype = do
  rootType <- defineJSONType datatype
  typeClassFetch <- deriveFetch argType (tName datatype) query
  args <- argumentTypes
  pure $ rootType <> typeClassFetch <> args

defineQueryD :: QueryD -> Q [Dec]
defineQueryD QueryD {queryTypes = rootType:subTypes, queryText, queryArgTypes} = do
  rootDecs <- defineOperationType (queryArgumentType queryArgTypes) queryText rootType
  subTypeDecs <- concat <$> mapM defineJSONType subTypes
  return $ rootDecs ++ subTypeDecs
defineQueryD QueryD {queryTypes = []} = return []

defineQuery :: IO (Validation DataTypeLib) -> (GQLQueryRoot, String) -> Q [Dec]
defineQuery ioSchema queryRoot = do
  schema <- runIO ioSchema
  case schema >>= (`validateWith` queryRoot) of
    Left errors  -> fail (renderGQLErrors errors)
    Right queryD -> defineQueryD queryD
