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
import qualified Data.HashMap.Lazy                      as H (lookup)
import           Data.Semigroup                         ((<>))
import           Data.Text                              (unpack)
import           Language.Haskell.TH

import           Data.Morpheus.Execution.Internal.Utils (nameSpaceTypeString)

--
-- MORPHEUS
import           Data.Morpheus.Types.Internal.Data      (DataField (..), isFieldNullable)
import           Data.Morpheus.Types.Internal.DataD     (ConsD (..), TypeD (..))
import           Data.Morpheus.Types.Internal.TH        (instanceFunD, instanceHeadT)

deriveFromJSON :: TypeD -> Q Dec
deriveFromJSON TypeD {tCons = []} = fail "Type Should Have at least one Constructor"
deriveFromJSON TypeD {tName, tNamespace, tCons = [cons]} = defineFromJSON name (aesonObject tNamespace) cons
  where
    name = nameSpaceTypeString tNamespace tName
deriveFromJSON typeD@TypeD {tName, tCons, tNamespace}
  | isEnum tCons = defineFromJSON name aesonEnum tCons
  | otherwise = defineFromJSON name (aesonUnionObject tNamespace) typeD
  where
    name = nameSpaceTypeString tNamespace tName

aesonObject :: [String] -> ConsD -> ExpQ
aesonObject tNamespace con@ConsD {cName} =
  appE [|withObject name|] (lamE [varP (mkName "o")] ((aesonObjectBody tNamespace) con))
  where
    name = nameSpaceTypeString tNamespace cName

aesonObjectBody :: [String] -> ConsD -> ExpQ
aesonObjectBody namespace ConsD {cName, cFields} = handleFields cFields
  where
    consName = mkName $ nameSpaceTypeString namespace cName
    ------------------------------------------
    handleFields [] = fail $ "No Empty Object"
    handleFields fields = startExp fields
    ----------------------------------------------------------------------------------
      -- Optional Field
      where
        defField field@DataField {fieldName}
          | isFieldNullable field = [|o .:? fName|]
          | otherwise = [|o .: fName|]
          where
            fName = unpack fieldName
            -------------------------------------------------------------------
        startExp fNames = uInfixE (conE consName) (varE '(<$>)) (applyFields fNames)
          where
            applyFields []     = fail "No Empty fields"
            applyFields [x]    = defField x
            applyFields (x:xs) = uInfixE (defField x) (varE '(<*>)) (applyFields xs)

aesonUnionObject :: [String] -> TypeD -> ExpQ
aesonUnionObject namespace TypeD {tCons} =
  appE (varE $ 'takeValueType) (lamCaseE ((map buildMatch tCons) <> [elseCaseEXP]))
  where
    buildMatch cons@ConsD {cName} = match pattern body []
      where
        pattern = tupP [litP (stringL cName), varP $ mkName "o"]
        body = normalB $ aesonObjectBody namespace cons

takeValueType :: ((String, Object) -> Parser a) -> Value -> Parser a
takeValueType f (Object hMap) =
  case H.lookup "__typename" hMap of
    Nothing         -> fail "key \"__typename\" not found on object"
    Just (String x) -> pure (unpack x, hMap) >>= f
    Just val        -> fail $ "key \"__typename\" should be string but found: " <> show val
takeValueType _ _ = fail $ "expected Object"

defineFromJSON :: String -> (t -> ExpQ) -> t -> DecQ
defineFromJSON tName parseJ cFields = instanceD (cxt []) iHead [method]
  where
    iHead = instanceHeadT ''FromJSON tName []
    -----------------------------------------
    method = instanceFunD 'parseJSON [] (parseJ cFields)

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
