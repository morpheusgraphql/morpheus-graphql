{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Input.Object
  ( validateInputObject
  , validateInput
  ) where

import           Data.Morpheus.Error.InputType       (typeMismatchMetaError)
import           Data.Morpheus.PreProcess.Utils      (existsInputObjectType, existsLeafType, fieldOf)
import           Data.Morpheus.Schema.Internal.Types (Core (..), Field (..), GObject (..), InputField (..), InputType,
                                                      Leaf (..), TypeLib)
import qualified Data.Morpheus.Schema.Internal.Types as T (InternalType (..))
import           Data.Morpheus.Types.Error           (MetaError (..), MetaValidation)
import           Data.Morpheus.Types.JSType          (JSType (..), ScalarValue (..))
import           Data.Morpheus.Types.MetaInfo        (MetaInfo (..), Position)
import           Data.Text                           as Text (Text)

validateLeaf :: MetaInfo -> JSType -> Leaf -> MetaValidation JSType
validateLeaf _ (Scalar x) (LScalar _) = pure (Scalar x) -- TODO Validate Scalar
validateLeaf _ (JSEnum x) (LEnum _ _) = pure (JSEnum x) -- TODO Validate Scalar
validateLeaf meta scalar _            = Left $ TypeMismatch meta scalar ""

validateScalarTypes :: MetaInfo -> Text -> ScalarValue -> MetaValidation ScalarValue
validateScalarTypes _ "String" (String x)   = pure (String x)
validateScalarTypes _ "Int" (Int x)         = pure (Int x)
validateScalarTypes _ "Boolean" (Boolean x) = pure (Boolean x)
validateScalarTypes meta _ scalar           = Left $ TypeMismatch meta (Scalar scalar) "TODO add Type"

validateFieldType :: MetaInfo -> JSType -> InputType -> MetaValidation JSType
validateFieldType meta (Scalar found) (T.Scalar core) = Scalar <$> validateScalarTypes meta (name core) found
validateFieldType _ (JSObject x) (T.Object _)         = pure (JSObject x) -- TODO Validate Scalar
validateFieldType _ (JSEnum x) (T.Enum _ _)           = pure (JSEnum x) -- TODO Validate Scalar
validateFieldType meta jsType _                       = Left $ TypeMismatch meta jsType "TODO add Type"

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

validateInput :: TypeLib -> InputType -> Position -> (Text, JSType) -> MetaValidation JSType
validateInput typeLib (T.Object oType) pos (_, JSObject fields) =
  JSObject <$> mapM (validateInputObject typeLib oType pos) fields
validateInput _ (T.Object (GObject _ core)) pos (_, jsType) = typeMismatchMetaError pos (name core) jsType
validateInput _ (T.Scalar core) pos (varName, x) = validateFieldType meta x (T.Scalar core)
  where
    meta = MetaInfo {typeName = name core, key = varName, position = pos}
validateInput _ (T.Enum _ core) pos (varName, x) = validateFieldType meta x (T.Scalar core)
  where
    meta = MetaInfo {typeName = name core, key = varName, position = pos}
