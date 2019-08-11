{-# LANGUAGE ConstrainedClassMethods #-}
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

-- import           Data.Aeson (eitherDecode)
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
  dec <- instanceD (cxt []) (appT (conT ''Fetch) (conT typeName)) [queryForF]
  return [dec]
  where
    queryForF = funD (mkName "queryFor") [clause [conP (mkName "Proxy") []] (normalB [|query|]) []]
    ----------------------------------------------------------------------------------------------------

--    fetchF =
--      funD (mkName "fetch") [clause [varP $ mkName "trans"] (normalB [|trans query >>= \x -> eitherDecode x|]) []]
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

defineRec :: RecType -> Q [Dec]
defineRec x = pure [buildRec $ recDefinition x]

buildRec :: (Name, [Con]) -> Dec
buildRec (name, cons) = DataD [] name [] Nothing cons $ map derive ["Show", "Generic", "FromJSON"]
  where
    derive className = DerivClause Nothing [ConT (mkName className)]

defineWithInstance :: String -> RecType -> Q [Dec]
defineWithInstance query (strName, fields) = do
  instDec <- fetchInstance typeName query
  pure (buildRec (typeName, cons) : instDec)
  where
    (typeName, cons) = recDefinition (strName, fields)
