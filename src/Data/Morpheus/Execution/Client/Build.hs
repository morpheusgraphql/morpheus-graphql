{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Data.Morpheus.Execution.Client.Build
  ( defineQuery
  ) where

import           Control.Lens                            (declareLenses)
import           Data.Semigroup                          ((<>))
import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Execution.Client.Aeson    (deriveFromJSON)
import           Data.Morpheus.Execution.Client.Compile  (validateWith)
import           Data.Morpheus.Execution.Client.Data     (AppD (..), ConsD (..), FieldD (..), QueryD (..), TypeD (..))
import           Data.Morpheus.Execution.Client.Fetch    (deriveFetch)
import           Data.Morpheus.Types.Internal.Data       (DataTypeLib)
import           Data.Morpheus.Types.Internal.Validation (Validation)
import           Data.Morpheus.Types.Types               (GQLQueryRoot (..))

queryArgumentType :: [TypeD] -> (Type, Q [Dec])
queryArgumentType [] = (ConT $ mkName "()", pure [])
queryArgumentType (rootType@TypeD {tName}:xs) = (ConT $ mkName tName, types)
  where
    types = pure $ map (defineType ["ToJSON"]) (rootType : xs)

defineType :: [String] -> TypeD -> Dec
defineType derivings TypeD {tName, tCons} =
  DataD [] (mkName tName) [] Nothing (map cons tCons) $ map derive (["Show", "Generic"] ++ derivings)
  where
    defBang = Bang NoSourceUnpackedness NoSourceStrictness
    derive className = DerivClause Nothing [ConT (mkName className)]
    cons ConsD {cName, cFields} = RecC (mkName cName) (map genField cFields)
      where
        genField FieldD {fieldNameD, fieldTypeD} = (mkName fieldNameD, defBang, genFieldT fieldTypeD)
          where
            genFieldT (ListD td)   = AppT (ConT ''[]) (genFieldT td)
            genFieldT (MaybeD td)  = AppT (ConT ''Maybe) (genFieldT td)
            genFieldT (BaseD name) = ConT (mkName name)

defineJSONType :: TypeD -> Q [Dec]
defineJSONType datatype = do
  record <- declareLenses (pure [defineType [] datatype])
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
  validate schema
  where
    validate x =
      case x of
        Left errors -> fail (show errors)
        Right schema ->
          case validateWith schema queryRoot of
            Left errors  -> fail (show errors)
            Right queryD -> defineQueryD queryD
