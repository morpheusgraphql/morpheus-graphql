{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Input.Object
  ( validateInputObject
  , validateInput
  ) where

import           Data.Morpheus.Error.Input           (InputError (..), InputErrorKind (..), Prop (..),
                                                      typeMismatchMetaError)
import           Data.Morpheus.PreProcess.Utils      (fieldOf, lookupType)
import           Data.Morpheus.Schema.Internal.Types (Core (..), Field (..), GObject (..), InputField (..), InputObject,
                                                      InputType, Leaf (..), TypeLib (..))
import qualified Data.Morpheus.Schema.Internal.Types as T (InternalType (..))
import           Data.Morpheus.Types.Error           (MetaError (..), MetaValidation)
import           Data.Morpheus.Types.JSType          (JSType (..), ScalarValue (..))
import           Data.Morpheus.Types.MetaInfo        (MetaInfo (..), Position)
import           Data.Text                           (Text)
import qualified Data.Text                           as T (intercalate)

leafToInputType :: Leaf -> InputType
leafToInputType (LScalar core) = T.Scalar core
leafToInputType (LEnum x y)    = T.Enum x y

type InputValidation a = Either InputError a

existsInputObjectType :: [Prop] -> JSType -> TypeLib -> Text -> InputValidation InputObject
existsInputObjectType path' jsType lib' = lookupType error' (inputObject lib')
  where
    error' = InputError {path = path', errorKind = UnexpectedType jsType}

existsLeafType :: [Prop] -> JSType -> TypeLib -> Text -> InputValidation Leaf
existsLeafType path' jsType lib' = lookupType error' (leaf lib')
  where
    error' = InputError {path = path', errorKind = UnexpectedType jsType}

convertError :: Position -> InputValidation a -> MetaValidation a
convertError position' (Left inpError) =
  case errorKind inpError of
    UnexpectedType jsType -> Left $ TypeMismatch meta jsType
    UndefinedField        -> Left $ UnknownField meta
  where
    meta = MetaInfo {position = position', typeName = "String", key = key'}
    key' = T.intercalate "." $ fmap propKey (path inpError)
convertError _ (Right x) = pure x

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

validateInputObject ::
     [Prop] -> TypeLib -> GObject InputField -> Position -> (Text, JSType) -> MetaValidation (Text, JSType)
validateInputObject prop' lib' (GObject parentFields _) pos (_name, JSObject fields) = do
  fieldTypeName' <- fieldType . unpackInputField <$> fieldOf (pos, _name) parentFields _name
  let currentProp = prop' ++ [Prop _name fieldTypeName']
  inputObject' <- convertError pos (existsInputObjectType currentProp (JSObject fields) lib' fieldTypeName')
  mapM (validateInputObject currentProp lib' inputObject' pos) fields >>= \x -> pure (_name, JSObject x)
validateInputObject prop' lib' (GObject parentFields core) pos (_name, jsType) = do
  fieldTypeName' <- fieldType . unpackInputField <$> fieldOf (pos, _name) parentFields _name
  let currentProp = prop' ++ [Prop _name fieldTypeName']
  fieldType' <- convertError pos (leafToInputType <$> existsLeafType currentProp jsType lib' fieldTypeName')
  validateFieldType meta fieldType' jsType >> pure (_name, jsType)
  where
    meta = MetaInfo {typeName = name core, key = _name, position = pos}

validateInput :: TypeLib -> InputType -> Position -> (Text, JSType) -> MetaValidation JSType
validateInput typeLib (T.Object oType) pos (key', JSObject fields) =
  JSObject <$> mapM (validateInputObject [Prop key' "TODO:"] typeLib oType pos) fields
validateInput _ (T.Object (GObject _ core)) pos (_, jsType) = typeMismatchMetaError pos (name core) jsType
validateInput _ (T.Scalar core) pos (varName, x) = validateFieldType meta (T.Scalar core) x
  where
    meta = MetaInfo {typeName = name core, key = varName, position = pos}
validateInput _ (T.Enum _ core) pos (varName, x) = validateFieldType meta (T.Scalar core) x
  where
    meta = MetaInfo {typeName = name core, key = varName, position = pos}
