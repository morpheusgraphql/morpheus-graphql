{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Input.Object
  ( validateInputObject
  , validateInputVariable
  ) where

import           Data.Morpheus.PreProcess.Utils      (existsInputObjectType, existsLeafType, fieldOf)
import           Data.Morpheus.Schema.Internal.Types (Core (..), Field (..), GObject (..), InputField (..), InputType,
                                                      Leaf (..), TypeLib)
import qualified Data.Morpheus.Schema.Internal.Types as T (InternalType (..))
import           Data.Morpheus.Types.Error           (MetaError (..), MetaValidation)
import           Data.Morpheus.Types.JSType          (JSType (..))
import           Data.Morpheus.Types.MetaInfo        (MetaInfo (..), Position)
import           Data.Text                           as Text (Text, pack)

subfieldsMustSelected :: Text -> MetaInfo -> MetaValidation a -- TODO: Real error
subfieldsMustSelected fName meta = Left $ TypeMismatch meta fName "Field"

validateLeaf :: MetaInfo -> JSType -> Leaf -> MetaValidation JSType
validateLeaf _ (Scalar x) (LScalar _) = pure (Scalar x) -- TODO Validate Scalar
validateLeaf _ (JSEnum x) (LEnum _ _) = pure (JSEnum x) -- TODO Validate Scalar
validateLeaf meta jsType _            = Left $ TypeMismatch meta (Text.pack $ show jsType) "TODO add Type"

validateFieldType :: MetaInfo -> JSType -> InputType -> MetaValidation JSType
validateFieldType _ (Scalar x) (T.Scalar _)   = pure (Scalar x) -- TODO Validate Scalar
 -- typeMismatch _ (Scalar (String x)) "String"   = pure (Scalar (String x))
-- typeMismatch _ (Scalar (Int x)) "Int"         = pure (Scalar (Int x))
-- typeMismatch _ (Scalar (Boolean x)) "Boolean" = pure (Scalar (Boolean x))
validateFieldType _ (JSObject x) (T.Object _) = pure (JSObject x) -- TODO Validate Scalar
validateFieldType _ (JSEnum x) (T.Enum _ _)   = pure (JSEnum x) -- TODO Validate Scalar
validateFieldType meta jsType _               = Left $ TypeMismatch meta (Text.pack $ show jsType) "TODO add Type"

validateInputObject :: TypeLib -> GObject InputField -> Position -> (Text, JSType) -> MetaValidation (Text, JSType)
validateInputObject lib' (GObject parentFields _) pos (_name, JSObject fields) = do
  fieldTypeName' <- fieldType . unpackInputField <$> fieldOf (pos, _name) parentFields _name
  inputObject' <- existsInputObjectType (pos, _name) fieldTypeName' lib'
  mapM (validateInputObject lib' inputObject' pos) fields >>= \x -> pure (_name, JSObject x)
validateInputObject lib' (GObject parentFields core) pos (_name, x) = do
  fieldTypeName' <- fieldType . unpackInputField <$> fieldOf (pos, _name) parentFields _name
  fieldType' <- existsLeafType (pos, _name) fieldTypeName' lib'
  validateLeaf meta x fieldType' >> pure (_name, x)
  where
    meta = MetaInfo {typeName = name core, key = _name, position = pos}

validateInputVariable :: TypeLib -> InputType -> Position -> (Text, JSType) -> MetaValidation JSType
validateInputVariable typeLib (T.Object oType) pos (_, JSObject fields) =
  JSObject <$> mapM (validateInputObject typeLib oType pos) fields
validateInputVariable _ (T.Object (GObject _ core)) pos (key_, _) = subfieldsMustSelected key_ meta
  where
    meta = MetaInfo {typeName = name core, key = key_, position = pos}
validateInputVariable _ _type pos (varName, x) = validateFieldType meta x _type
  where
    meta = MetaInfo {typeName = "", key = varName, position = pos}
