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

leafToInputType :: Leaf -> InputType
leafToInputType (LScalar core) = T.Scalar core
leafToInputType (LEnum x y)    = T.Enum x y

validateScalarTypes :: MetaInfo -> Text -> ScalarValue -> MetaValidation ScalarValue
validateScalarTypes _ "String" (String x)   = pure (String x)
validateScalarTypes meta "String" scalar    = Left $ TypeMismatch (meta {typeName = "String"}) (Scalar scalar)
validateScalarTypes _ "Int" (Int x)         = pure (Int x)
validateScalarTypes meta "Int" scalar       = Left $ TypeMismatch (meta {typeName = "Int"}) (Scalar scalar)
validateScalarTypes _ "Boolean" (Boolean x) = pure (Boolean x)
validateScalarTypes meta "Boolean" scalar   = Left $ TypeMismatch (meta {typeName = "Boolean"}) (Scalar scalar)
validateScalarTypes _ _ scalar              = pure scalar

validateFieldType :: MetaInfo -> InputType -> JSType -> MetaValidation JSType
validateFieldType meta (T.Scalar core) (Scalar found) = Scalar <$> validateScalarTypes meta (name core) found
validateFieldType _ (T.Object _) (JSObject x)         = pure (JSObject x) -- TODO Validate Scalar
validateFieldType _ (T.Enum _ _) (JSEnum x)           = pure (JSEnum x) -- TODO Validate Scalar
validateFieldType meta _ jsType                       = Left $ TypeMismatch meta jsType

validateInputObject :: TypeLib -> GObject InputField -> Position -> (Text, JSType) -> MetaValidation (Text, JSType)
validateInputObject lib' (GObject parentFields _) pos (_name, JSObject fields) = do
  fieldTypeName' <- fieldType . unpackInputField <$> fieldOf (pos, _name) parentFields _name
  inputObject' <- existsInputObjectType (pos, _name) fieldTypeName' lib'
  mapM (validateInputObject lib' inputObject' pos) fields >>= \x -> pure (_name, JSObject x)
validateInputObject lib' (GObject parentFields core) pos (_name, x) = do
  fieldTypeName' <- fieldType . unpackInputField <$> fieldOf (pos, _name) parentFields _name
  fieldType' <- leafToInputType <$> existsLeafType (pos, _name) fieldTypeName' lib'
  validateFieldType meta fieldType' x >> pure (_name, x)
  where
    meta = MetaInfo {typeName = name core, key = _name, position = pos}

validateInput :: TypeLib -> InputType -> Position -> (Text, JSType) -> MetaValidation JSType
validateInput typeLib (T.Object oType) pos (_, JSObject fields) =
  JSObject <$> mapM (validateInputObject typeLib oType pos) fields
validateInput _ (T.Object (GObject _ core)) pos (_, jsType) = typeMismatchMetaError pos (name core) jsType
validateInput _ (T.Scalar core) pos (varName, x) = validateFieldType meta (T.Scalar core) x
  where
    meta = MetaInfo {typeName = name core, key = varName, position = pos}
validateInput _ (T.Enum _ core) pos (varName, x) = validateFieldType meta (T.Scalar core) x
  where
    meta = MetaInfo {typeName = name core, key = varName, position = pos}
