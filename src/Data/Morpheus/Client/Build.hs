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

import           Control.Lens              (declareLenses)
import           Data.Aeson
import           Data.ByteString.Lazy      (ByteString)
import           Data.Morpheus.Client.Data (AppD (..), ConsD (..), FieldD (..), QueryD (..), TypeD (..))
import           Data.Morpheus.Types.IO    (GQLRequest (..))
import           Data.Text                 (pack)
import           Language.Haskell.TH

queryArgumentType :: [TypeD] -> (Type, Q [Dec])
queryArgumentType [] = (ConT $ mkName "()", pure [])
queryArgumentType (rootType@TypeD {tName}:xs) = (ConT $ mkName tName, types)
  where
    types = pure $ map defType (rootType : xs)

defineQuery :: QueryD -> Q [Dec]
defineQuery QueryD {queryTypes = rootType:types, queryText, queryArgTypes} = do
  rootDecs <- rootDec
  subTypeDecs <- concat <$> mapM defineRec types
  return $ rootDecs ++ subTypeDecs
  where
    rootDec = defineWithInstance (queryArgumentType queryArgTypes) queryText rootType
defineQuery QueryD {queryTypes = []} = return []

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

instanceFromJSON :: TypeD -> Q [Dec]
instanceFromJSON TypeD {tCons = []} = fail "Type Should Have at least one Constructor"
instanceFromJSON TypeD {tName, tCons = [ConsD {cFields}]} =
  pure <$> instanceD (cxt []) (appT (conT ''FromJSON) (conT typeName)) [fromJson]
  where
    typeName = mkName tName
    fromJson = funD (mkName "parseJSON") [clause [] (normalB parseJ) []]
      where
        parseJ = appE [|withObject tName|] (lamE [varP (mkName "o")] (startExp cFields))
        ----------------------------------------------------------------------------------
        -- Optional Field
        defField FieldD {fieldNameD, fieldTypeD = MaybeD _} = [|o .:? fieldNameD|]
        -- Required Field
        defField FieldD {fieldNameD}                        = [|o .: fieldNameD|]
        -------------------------------------------------------------------
        startExp [] = conE typeName
        startExp fNames = uInfixE (conE typeName) (varE '(<$>)) (applyFields fNames)
          where
            applyFields []     = fail "No Empty fields"
            applyFields [x]    = defField x
            applyFields (x:xs) = uInfixE (defField x) (varE '(<*>)) (applyFields xs)
instanceFromJSON TypeD {} = fail "<TODO> write Mutliple Types"

defType :: TypeD -> Dec
defType TypeD {tName, tCons} = DataD [] typeName [] Nothing (map cons tCons) $ map derive ["Show", "Generic"]
  where
    typeName = mkName tName
    defBang = Bang NoSourceUnpackedness NoSourceStrictness
    derive className = DerivClause Nothing [ConT (mkName className)]
    cons ConsD {cFields} = RecC typeName (map genField cFields)
      where
        genField FieldD {fieldNameD, fieldTypeD} = (mkName fieldNameD, defBang, genFieldT fieldTypeD)
          where
            genFieldT (ListD td)   = AppT (ConT $ mkName "[]") (genFieldT td)
            genFieldT (MaybeD td)  = AppT (ConT $ mkName "Maybe") (genFieldT td)
            genFieldT (BaseD name) = ConT (mkName name)

defineRec :: TypeD -> Q [Dec]
defineRec x = do
  record <- declareLenses (pure [defType x])
  toJson <- instanceFromJSON x
  pure $ record <> toJson

defineWithInstance :: (Type, Q [Dec]) -> String -> TypeD -> Q [Dec]
defineWithInstance (argType, argumentTypes) query datatype = do
  record <- declareLenses (pure [defType datatype])
  toJson <- instanceFromJSON datatype
  args <- argumentTypes
  instDec <- instanceFetch argType (mkName $ tName datatype) query
  pure $ record <> toJson <> instDec <> args
