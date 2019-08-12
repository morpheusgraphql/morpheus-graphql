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

defineQuery :: QueryD -> Q [Dec]
defineQuery QueryD {queryTypes = rootType:types, queryText} = do
  rootDecs <- rootDec
  subTypeDecs <- concat <$> mapM defineRec types
  return $ rootDecs ++ subTypeDecs
  where
    rootDec = defineWithInstance queryText rootType
defineQuery QueryD {queryTypes = []} = return []

class Fetch a where
  type Args a :: *
  type Args a = ()
  __fetch :: (Monad m, FromJSON a) => String -> (String -> m ByteString) -> Args a -> m (Either String a)
  __fetch query trans _args = eitherDecode <$> trans query
  fetch :: (Monad m, FromJSON a) => (String -> m ByteString) -> Args a -> m (Either String a)

defineInstanceFetch :: Name -> String -> Q [Dec]
defineInstanceFetch typeName query = pure <$> instanceD (cxt []) (appT (conT ''Fetch) (conT typeName)) methods
  where
    methods = [funD (mkName "fetch") [clause [] (normalB [|__fetch query|]) []]]

defineInstanceFromJSON :: TypeD -> Q [Dec]
defineInstanceFromJSON TypeD {tCons = []} = fail "No Multiple Types"
defineInstanceFromJSON TypeD {tName, tCons = [ConsD {cFields}]} =
  pure <$> instanceD (cxt []) (appT (conT ''FromJSON) (conT typeName)) [fromJson]
  where
    typeName = mkName tName
    fromJson = funD (mkName "parseJSON") [clause [] (normalB parseJ) []]
      where
        parseJ = appE [|withObject tName|] (lamE [varP (mkName "o")] (startExp (map fieldNameD cFields)))
          where
            defField n = [|o .: n|]
            startExp [] = conE typeName
            startExp fNames = uInfixE (conE typeName) (varE '(<*>)) (applyFields fNames)
              where
                applyFields []     = fail "No Empty fields"
                applyFields [x]    = defField x
                applyFields (x:xs) = uInfixE (defField x) (varE '(<$>)) (applyFields xs)
defineInstanceFromJSON TypeD {} = fail "No Multiple Types"

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
  toJson <- defineInstanceFromJSON x
  pure $ record <> toJson

defineWithInstance :: String -> TypeD -> Q [Dec]
defineWithInstance query datatype = do
  record <- declareLenses (pure [defType datatype])
  toJson <- defineInstanceFromJSON datatype
  instDec <- defineInstanceFetch (mkName $ tName datatype) query
  pure $ record <> toJson <> instDec
