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
import qualified Data.HashMap.Lazy         as H (lookup)
import           Data.Morpheus.Client.Data (AppD (..), ConsD (..), FieldD (..), QueryD (..), TypeD (..))
import           Data.Morpheus.Types.IO    (GQLRequest (..))
import           Data.Text                 (pack, unpack)
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

aesonObject :: ConsD -> ExpQ
aesonObject ConsD {cName, cFields} = handleFields cFields
  where
    consName = mkName cName
    handleFields [] = fail $ "No Empty Object"
    handleFields fields = appE [|withObject cName|] (lamE [varP (mkName "o")] (startExp fields))
    ----------------------------------------------------------------------------------
         -- Optional Field
      where
        defField FieldD {fieldNameD, fieldTypeD = MaybeD _} = [|o .:? fieldNameD|]
        -- Required Field
        defField FieldD {fieldNameD}                        = [|o .: fieldNameD|]
            -------------------------------------------------------------------
        startExp fNames = uInfixE (conE consName) (varE '(<$>)) (applyFields fNames)
          where
            applyFields []     = fail "No Empty fields"
            applyFields [x]    = defField x
            applyFields (x:xs) = uInfixE (defField x) (varE '(<*>)) (applyFields xs)

aesonEnum :: [ConsD] -> ExpQ
aesonEnum cons = lamCaseE handlers
  where
    handlers = (map buildMatch cons) <> [elseCaseEXP]
      where
        buildMatch ConsD {cName} = match pattern body []
          where
            pattern = litP $ stringL cName
            body = normalB $ appE (varE $ mkName "pure") (conE $ mkName cName)

elseCaseEXP :: MatchQ
elseCaseEXP = match (varP varName) body []
  where
    varName = mkName "invalidValue"
    body =
      normalB $
      appE
        (varE $ mkName "fail")
        (uInfixE (appE (varE 'show) (varE varName)) (varE '(<>)) (stringE $ " is Not Valid Union Constructor"))

takeValueType :: Value -> Either String (String, Value)
takeValueType (Object hMap) =
  case H.lookup "__typename" hMap of
    Nothing         -> Left "key \"__typename\" not found on object"
    Just (String x) -> pure (unpack x, Object hMap)
    Just val        -> Left $ "key \"__typename\" should be string but found: " <> show val
takeValueType _ = Left $ "expected Object"

aesonUnionObject :: TypeD -> ExpQ
aesonUnionObject TypeD {tCons} = appE (lamCaseE ((map buildMatch tCons) <> [elseCaseEXP])) (varE 'takeValueType)
  where
    buildMatch ConsD {cName} = match pattern body []
      where
        pattern = tupP [litP (stringL cName), varP $ mkName "o"]
        body = normalB $ appE (varE 'pure) (conE $ mkName cName)

defineFromJSON :: String -> (t -> ExpQ) -> t -> DecQ
defineFromJSON tName func inp =
  instanceD (cxt []) (appT (conT ''FromJSON) (conT $ mkName tName)) [parseJSONExp func inp]
  where
    parseJSONExp :: (t -> ExpQ) -> t -> DecQ
    parseJSONExp parseJ cFields = funD 'parseJSON [clause [] (normalB $ parseJ cFields) []]

isEnum :: [ConsD] -> Bool
isEnum = not . isEmpty . filter (isEmpty . cFields)
  where
    isEmpty = (0 ==) . length

instanceFromJSON :: TypeD -> Q Dec
instanceFromJSON TypeD {tCons = []} = fail "Type Should Have at least one Constructor"
instanceFromJSON TypeD {tName, tCons = [cons]} = defineFromJSON tName aesonObject cons
instanceFromJSON typeD@TypeD {tName, tCons}
  | isEnum tCons = defineFromJSON tName aesonEnum tCons
  | otherwise = defineFromJSON tName aesonUnionObject typeD

-- =
defType :: TypeD -> Dec
defType TypeD {tName, tCons} = DataD [] (mkName tName) [] Nothing (map cons tCons) $ map derive ["Show", "Generic"]
  where
    defBang = Bang NoSourceUnpackedness NoSourceStrictness
    derive className = DerivClause Nothing [ConT (mkName className)]
    cons ConsD {cName, cFields} = RecC (mkName cName) (map genField cFields)
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
  pure $ record <> [toJson]

defineWithInstance :: (Type, Q [Dec]) -> String -> TypeD -> Q [Dec]
defineWithInstance (argType, argumentTypes) query datatype = do
  record <- declareLenses (pure [defType datatype])
  toJson <- instanceFromJSON datatype
  args <- argumentTypes
  instDec <- instanceFetch argType (mkName $ tName datatype) query
  pure $ record <> [toJson] <> instDec <> args
