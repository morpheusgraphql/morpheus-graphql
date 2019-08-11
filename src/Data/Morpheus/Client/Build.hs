{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE TypeApplications        #-}

module Data.Morpheus.Client.Build
  ( defineQuery
  , Fetch(..)
  ) where

import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Proxy
import           Language.Haskell.TH

defineQuery :: ([(String, [(String, String)])], String) -> Q [Dec]
defineQuery (rootType:types, query) = do
  rootDecs <- rootDec
  subTypeDecs <- concat <$> mapM defineRec types
  return $ rootDecs ++ subTypeDecs
  where
    rootDec = defineWithInstance query rootType
defineQuery ([], _) = return []

class Fetch a where
  queryFor :: Proxy a -> String
  fetch :: (Monad m, FromJSON a) => (String -> m ByteString) -> m (Either String a)
  fetch trans = eitherDecode <$> trans (queryFor (Proxy :: Proxy a))

fetchInstance :: Name -> String -> Q [Dec]
fetchInstance typeName query = do
  dec <- instanceD (cxt []) (appT (conT ''Fetch) (conT typeName)) [queryFor']
  return [dec]
  where
    queryFor' = funD (mkName "queryFor") [clause [varP (mkName "_")] (normalB [|query|]) []]

aesonObjectInstance :: RecType -> Q [Dec]
aesonObjectInstance (name, fields) = do
  dec <- instanceD (cxt []) (appT (conT ''FromJSON) (conT typeName)) [fromJson]
  return [dec]
  where
    typeName = mkName name
    fieldNames = map fst fields
    fromJson = funD (mkName "parseJSON") [clause [] (normalB parseJ) []]
      where
        parseJ = appE [|withObject name|] (lamE [varP (mkName "o")] (startExp fieldNames))
          where
            defField n = [|o .: n|]
            liftTH = varE '(<*>)
            mapTH = varE '(<$>)
            startExp [] = conE typeName
            startExp fNames = uInfixE (conE typeName) mapTH (applyFields fNames)
              where
                applyFields []     = fail "No Empty fields"
                applyFields [x]    = defField x
                applyFields (x:xs) = uInfixE (defField x) liftTH (applyFields xs)

type RecType = (String, [(String, String)])

recDefinition :: RecType -> (Name, [Con])
recDefinition (strName, fields) = (typeName, [recordCon])
  where
    typeName = mkName strName
    defBang = Bang NoSourceUnpackedness NoSourceStrictness
    recordCon = RecC typeName (map genField fields)
      where
        genField (fieldName, fType) = (mkName fieldName, defBang, ConT $ mkName fType)
          where
            _name name = "_" <> name

buildRec :: (Name, [Con]) -> Dec
buildRec (name, cons) = DataD [] name [] Nothing cons $ map derive ["Show", "Generic"]
  where
    derive className = DerivClause Nothing [ConT (mkName className)]

-------------------------------
defineRec :: RecType -> Q [Dec]
defineRec x = do
  toJson <- aesonObjectInstance x
  pure $ buildRec (recDefinition x) : toJson

defineWithInstance :: String -> RecType -> Q [Dec]
defineWithInstance query recType = do
  instDec <- fetchInstance typeName query
  toJson <- aesonObjectInstance recType
  pure $ buildRec (typeName, cons) : toJson <> instDec
  where
    (typeName, cons) = recDefinition recType
