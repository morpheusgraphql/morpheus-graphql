{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}

module Data.Morpheus.Execution.Client.Aeson
  ( deriveFromJSON
  , takeValueType
  ) where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Lazy                   as H (lookup)
import           Data.Semigroup                      ((<>))
import           Data.Text                           (unpack)
import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Execution.Client.Data (AppD (..), ConsD (..), FieldD (..), TypeD (..))

deriveFromJSON :: TypeD -> Q Dec
deriveFromJSON TypeD {tCons = []} = fail "Type Should Have at least one Constructor"
deriveFromJSON TypeD {tName, tCons = [cons]} = defineFromJSON tName aesonObject cons
deriveFromJSON typeD@TypeD {tName, tCons}
  | isEnum tCons = defineFromJSON tName aesonEnum tCons
  | otherwise = defineFromJSON tName aesonUnionObject typeD

aesonObject :: ConsD -> ExpQ
aesonObject con@ConsD {cName} = appE [|withObject cName|] (lamE [varP (mkName "o")] (aesonObjectBody con))

aesonObjectBody :: ConsD -> ExpQ
aesonObjectBody ConsD {cName, cFields} = handleFields cFields
  where
    consName = mkName cName
    handleFields [] = fail $ "No Empty Object"
    handleFields fields = startExp fields
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

aesonUnionObject :: TypeD -> ExpQ
aesonUnionObject TypeD {tCons} = appE (varE $ 'takeValueType) (lamCaseE ((map buildMatch tCons) <> [elseCaseEXP]))
  where
    buildMatch cons@ConsD {cName} = match pattern body []
      where
        pattern = tupP [litP (stringL cName), varP $ mkName "o"]
        body = normalB (aesonObjectBody cons)

takeValueType :: ((String, Object) -> Parser a) -> Value -> Parser a
takeValueType f (Object hMap) =
  case H.lookup "__typename" hMap of
    Nothing         -> fail "key \"__typename\" not found on object"
    Just (String x) -> pure (unpack x, hMap) >>= f
    Just val        -> fail $ "key \"__typename\" should be string but found: " <> show val
takeValueType _ _ = fail $ "expected Object"

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

aesonEnum :: [ConsD] -> ExpQ
aesonEnum cons = lamCaseE handlers
  where
    handlers = (map buildMatch cons) <> [elseCaseEXP]
      where
        buildMatch ConsD {cName} = match pattern body []
          where
            pattern = litP $ stringL cName
            body = normalB $ appE (varE 'pure) (conE $ mkName cName)

elseCaseEXP :: MatchQ
elseCaseEXP = match (varP varName) body []
  where
    varName = mkName "invalidValue"
    body =
      normalB $
      appE
        (varE $ mkName "fail")
        (uInfixE (appE (varE 'show) (varE varName)) (varE '(<>)) (stringE $ " is Not Valid Union Constructor"))
