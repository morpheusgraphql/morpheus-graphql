{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Morpheus.Execution.Document.Decode
  ( deriveDecode
  ) where

import           Data.Text                               (Text)
import           Language.Haskell.TH

import           Data.Morpheus.Error.Internal            (internalArgumentError, internalTypeMismatch)

--
-- MORPHEUS
import           Data.Morpheus.Execution.Server.Decode   (Decode (..), DecodeObject (..))
import           Data.Morpheus.Types.Internal.DataD      (ConsD (..), FieldD (..), TypeD (..))
import           Data.Morpheus.Types.Internal.Validation (Validation)
import           Data.Morpheus.Types.Internal.Value      (Value (..))

objectBody :: ConsD -> ExpQ
objectBody ConsD {cName, cFields} = handleFields cFields
  where
    consName = mkName cName
    ----------------------------------------------------------------------------------
    handleFields fNames = uInfixE (conE consName) (varE '(<$>)) (applyFields fNames)
      where
        applyFields []     = fail "No Empty fields"
        applyFields [x]    = defField x
        applyFields (x:xs) = uInfixE (defField x) (varE '(<*>)) (applyFields xs)
            ----------
        defField FieldD {fieldNameD} = [|o .: fieldNameD|]

(.:) :: Decode a => Value -> Text -> Validation a
(Object object) .: selectorName = selectFromObject
  where
    selectFromObject =
      case lookup selectorName object of
        Nothing    -> internalArgumentError ("Missing Field: \"" <> selectorName <> "\"")
        Just value -> decode value
isType .: _ = internalTypeMismatch "InputObject" isType

deriveDecode :: TypeD -> Q [Dec]
deriveDecode TypeD {tName, tCons = [cons]} = pure <$> instanceD (cxt []) appHead methods
  where
    appHead = appT classT typeT
      where
        classT = conT ''DecodeObject
        typeT = conT $ mkName tName
    methods = [funD 'decodeObject [clause argsE (normalB body) []]]
      where
        argsE = [varP (mkName "o")]
        body = objectBody cons
deriveDecode _ = pure []
