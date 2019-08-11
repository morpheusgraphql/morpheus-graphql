{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Client.Build
  ( defineQuery
  , Fetch(..)
  ) where

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

fetchInstance :: Name -> String -> Q [Dec]
fetchInstance typeName queryText = do
  dec <- instanceD (cxt []) (appT (conT ''Fetch) (conT typeName)) (map defineFunc methods)
  return [dec]
  where
    methods = [(mkName "queryFor", [|queryText|])]
    --------------------------------------------------------
    defineFunc (name, funcBody) = funD name [genClause funcBody]
    -------------------------------------------------------------------------------
    genClause funcBody = clause [conP (mkName "Proxy") []] (normalB funcBody) []

type RecType = (String, [(String, String)])

recDefinition :: RecType -> (Name, [Con])
recDefinition (strName, fields) = (typeName, [recordCon])
  where
    typeName = mkName strName
    defBang = Bang NoSourceUnpackedness NoSourceStrictness
    recordCon = RecC typeName (map genField fields)
      where
        genField (fieldName, fType) = (mkName $ _name fieldName, defBang, ConT $ mkName fType)
          where
            _name name = "_" <> name

defineRec :: RecType -> Q [Dec]
defineRec x = pure [buildRec $ recDefinition x]

buildRec :: (Name, [Con]) -> Dec
buildRec (name, cons) = DataD [] name [] Nothing cons []

defineWithInstance :: String -> RecType -> Q [Dec]
defineWithInstance query (strName, fields) = do
  instDec <- fetchInstance typeName query
  pure (buildRec (typeName, cons) : instDec)
  where
    (typeName, cons) = recDefinition (strName, fields)
