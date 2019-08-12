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
import           Data.Morpheus.Client.Data (ConsD (..), FieldD (..), QueryD (..), TypeD (..))
import           Language.Haskell.TH

queryArgumentType :: [TypeD] -> Type
queryArgumentType [] = ConT $ mkName "()"
queryArgumentType _  = ConT $ mkName "TestType"

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
  __fetch :: (Monad m, FromJSON a) => String -> (String -> m ByteString) -> Args a -> m (Either String a)
  __fetch query trans _args = eitherDecode <$> trans query
  fetch :: (Monad m, FromJSON a) => (String -> m ByteString) -> Args a -> m (Either String a)

instanceFetch :: Type -> Name -> String -> Q [Dec]
instanceFetch argumentType typeName query = pure <$> instanceD (cxt []) (appT (conT ''Fetch) (conT typeName)) methods
  where
    methods =
      [ funD (mkName "fetch") [clause [] (normalB [|__fetch query|]) []]
      , pure $ TySynInstD (mkName "Args") (TySynEqn [ConT typeName] argumentType)
      ]

instanceFromJSON :: TypeD -> Q [Dec]
instanceFromJSON TypeD {tCons = []} = fail "No Multiple Types"
instanceFromJSON TypeD {tName, tCons = [ConsD {cFields}]} =
  pure <$> instanceD (cxt []) (appT (conT ''FromJSON) (conT typeName)) [fromJson]
  where
    typeName = mkName tName
    fromJson = funD (mkName "parseJSON") [clause [] (normalB parseJ) []]
      where
        parseJ = appE [|withObject tName|] (lamE [varP (mkName "o")] (startExp (map fieldNameD cFields)))
          where
            defField n = [|o .: n|]
            startExp [] = conE typeName
            startExp fNames = uInfixE (conE typeName) (varE '(<$>)) (applyFields fNames)
              where
                applyFields []     = fail "No Empty fields"
                applyFields [x]    = defField x
                applyFields (x:xs) = uInfixE (defField x) (varE '(<*>)) (applyFields xs)
instanceFromJSON TypeD {} = fail "No Multiple Types"

defType :: TypeD -> Dec
defType TypeD {tName, tCons} = DataD [] typeName [] Nothing (map cons tCons) $ map derive ["Show", "Generic"]
  where
    typeName = mkName tName
    defBang = Bang NoSourceUnpackedness NoSourceStrictness
    derive className = DerivClause Nothing [ConT (mkName className)]
    cons ConsD {cFields} = RecC typeName (map genField cFields)
      where
        genField FieldD {fieldNameD, fieldTypeD} = (mkName fieldNameD, defBang, ConT $ mkName fieldTypeD)

defineRec :: TypeD -> Q [Dec]
defineRec x = do
  record <- declareLenses (pure [defType x])
  toJson <- instanceFromJSON x
  pure $ record <> toJson

defineWithInstance :: Type -> String -> TypeD -> Q [Dec]
defineWithInstance typeInstName query datatype = do
  record <- declareLenses (pure [defType datatype])
  toJson <- instanceFromJSON datatype
  instDec <- instanceFetch typeInstName (mkName $ tName datatype) query
  pure $ record <> toJson <> instDec
