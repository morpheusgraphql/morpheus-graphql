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
import           Data.Morpheus.Types.Internal.Value      (Object)
import           Data.Morpheus.Execution.Internal.Decode (withObject,decodeFieldWith)

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

(.:) :: Decode a => Object -> Text -> Validation a
object .: selectorName = decodeFieldWith decode selectorName object

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
