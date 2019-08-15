{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}

module Data.Morpheus.Client.Build
  ( defineQuery
  , Fetch(..)
  ) where

import           Control.Lens               (declareLenses)
import           Data.Aeson
import           Data.ByteString.Lazy       (ByteString)
import           Data.Morpheus.Client.Aeson (deriveFromJSON)
import           Data.Morpheus.Client.Data  (AppD (..), ConsD (..), FieldD (..), QueryD (..), TypeD (..))
import           Data.Morpheus.Types.IO     (GQLRequest (..))
import           Data.Text                  (pack)
import           Language.Haskell.TH

queryArgumentType :: [TypeD] -> (Type, Q [Dec])
queryArgumentType [] = (ConT $ mkName "()", pure [])
queryArgumentType (rootType@TypeD {tName}:xs) = (ConT $ mkName tName, types)
  where
    types = pure $ map defType (rootType : xs)

class Fetch a where
  type Args a :: *
  __fetch :: (Monad m, FromJSON a) => String -> (ByteString -> m ByteString) -> Args a -> m (Either String a)
  __fetch strQuery trans _variables = eitherDecode <$> trans (encode gqlReq)
    where
      gqlReq = GQLRequest {operationName = Just "<TODO>", query = pack strQuery, variables = Nothing}
  fetch :: (Monad m, FromJSON a) => (ByteString -> m ByteString) -> Args a -> m (Either String a)

instanceFetch :: Type -> Name -> String -> Q [Dec]
instanceFetch argumentType typeName query = pure <$> instanceD (cxt []) (appT (conT ''Fetch) (conT typeName)) methods
  where
    methods =
      [ funD (mkName "fetch") [clause [] (normalB [|__fetch query|]) []]
      , pure $ TySynInstD ''Args (TySynEqn [ConT typeName] argumentType)
      ]

defType :: TypeD -> Dec
defType TypeD {tName, tCons} = DataD [] (mkName tName) [] Nothing (map cons tCons) $ map derive ["Show", "Generic"]
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

defineType :: TypeD -> Q [Dec]
defineType x = do
  record <- declareLenses (pure [defType x])
  toJson <- pure <$> deriveFromJSON x
  pure $ record <> toJson

defineWithInstance :: (Type, Q [Dec]) -> String -> TypeD -> Q [Dec]
defineWithInstance (argType, argumentTypes) query datatype = do
  record <- declareLenses (pure [defType datatype])
  toJson <- deriveFromJSON datatype
  args <- argumentTypes
  instDec <- instanceFetch argType (mkName $ tName datatype) query
  pure $ record <> [toJson] <> instDec <> args

defineQuery :: QueryD -> Q [Dec]
defineQuery QueryD {queryTypes = rootType:types, queryText, queryArgTypes} = do
  rootDecs <- defineWithInstance (queryArgumentType queryArgTypes) queryText rootType
  subTypeDecs <- concat <$> mapM defineType types
  return $ rootDecs ++ subTypeDecs
defineQuery QueryD {queryTypes = []} = return []
