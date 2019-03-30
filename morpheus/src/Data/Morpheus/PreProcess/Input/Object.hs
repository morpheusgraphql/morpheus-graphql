{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Input.Object
  ( validateInputObject
  , validateInputVariable
  ) where

import           Data.Morpheus.PreProcess.Utils      (inputTypeBy)
import           Data.Morpheus.Schema.Internal.Types (Core (..), GObject (..), InputField (..), InputType, TypeLib)
import qualified Data.Morpheus.Schema.Internal.Types as T (InternalType (..))
import           Data.Morpheus.Types.Error           (MetaError (..), MetaValidation)
import           Data.Morpheus.Types.JSType          (JSType (..))
import           Data.Morpheus.Types.MetaInfo        (MetaInfo (..), Position)
import           Data.Text                           as Text (Text, pack)

fieldIsNotObject :: MetaInfo -> Core -> MetaValidation a -- TODO: real error
fieldIsNotObject meta core = Left $ TypeMismatch meta (name core) "ObJECT"

subfieldsMustSelected :: Text -> MetaInfo -> MetaValidation a -- TODO: Real error
subfieldsMustSelected fName meta = Left $ TypeMismatch meta fName "Field"

validateFieldType :: MetaInfo -> JSType -> InputType -> MetaValidation JSType
validateFieldType _ (Scalar x) (T.Scalar _)   = pure (Scalar x) -- TODO Validate Scalar
 -- typeMismatch _ (Scalar (String x)) "String"   = pure (Scalar (String x))
-- typeMismatch _ (Scalar (Int x)) "Int"         = pure (Scalar (Int x))
-- typeMismatch _ (Scalar (Boolean x)) "Boolean" = pure (Scalar (Boolean x))
validateFieldType _ (JSObject x) (T.Object _) = pure (JSObject x) -- TODO Validate Scalar
validateFieldType _ (JSEnum x) (T.Enum _ _)   = pure (JSEnum x) -- TODO Validate Scalar
validateFieldType meta jsType _               = Left $ TypeMismatch meta (Text.pack $ show jsType) "TODO add Type"

validateInputObject :: TypeLib -> GObject InputField -> Position -> (Text, JSType) -> MetaValidation (Text, JSType)
validateInputObject typeLib (GObject parentFields core) pos (_name, JSObject fields) = do
  typeSD <- inputTypeBy pos typeLib parentFields _name
  case typeSD of
    T.Object gObj  -> mapM (validateInputObject typeLib gObj pos) fields >>= \x -> pure (_name, JSObject x)
    T.Scalar core' -> fieldIsNotObject meta core'
    T.Enum _ core' -> fieldIsNotObject meta core'
  where
    meta = MetaInfo {typeName = name core, key = _name, position = pos}
validateInputObject typeLib (GObject parentFields core) pos (_key, x) = do
  fType <- inputTypeBy pos typeLib parentFields _key
  case fType of
    T.Object _ -> subfieldsMustSelected _key meta
    _          -> validateFieldType meta x fType >> pure (_key, x)
  where
    meta = MetaInfo {typeName = name core, key = _key, position = pos}

validateInputVariable :: TypeLib -> InputType -> Position -> (Text, JSType) -> MetaValidation JSType
validateInputVariable typeLib (T.Object oType) pos (_, JSObject fields) =
  JSObject <$> mapM (validateInputObject typeLib oType pos) fields
validateInputVariable _ (T.Object (GObject _ core)) pos (key_, _) = subfieldsMustSelected key_ meta
  where
    meta = MetaInfo {typeName = name core, key = key_, position = pos}
validateInputVariable _ _type pos (varName, x) = validateFieldType meta x _type
  where
    meta = MetaInfo {typeName = "", key = varName, position = pos}
