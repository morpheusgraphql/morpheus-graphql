{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Input.Object
  ( validateInputObject
  , validateInputValue
  ) where

import           Data.Morpheus.Error.Input           (InputError (..), InputValidation, Prop (..))
import           Data.Morpheus.PreProcess.Input.Enum (validateEnum)
import           Data.Morpheus.PreProcess.Utils      (lookupField, lookupType)
import           Data.Morpheus.Schema.Internal.Types (Core (..), Field (..), GObject (..), InputField (..), InputObject,
                                                      InputType, Leaf (..), TypeLib (..))
import qualified Data.Morpheus.Schema.Internal.Types as T (InternalType (..))
import           Data.Morpheus.Types.JSType          (JSType (..), ScalarValue (..))
import           Data.Text                           (Text)

generateError :: JSType -> Text -> [Prop] -> InputError
generateError jsType expected' path' = UnexpectedType path' expected' jsType

existsInputObjectType :: InputError -> TypeLib -> Text -> InputValidation InputObject
existsInputObjectType error' lib' = lookupType error' (inputObject lib')

existsLeafType :: InputError -> TypeLib -> Text -> InputValidation Leaf
existsLeafType error' lib' = lookupType error' (leaf lib')

validateScalarTypes :: Text -> ScalarValue -> [Prop] -> InputValidation ScalarValue
validateScalarTypes "String" (String x)   = pure . const (String x)
validateScalarTypes "String" scalar       = Left . generateError (Scalar scalar) "String"
validateScalarTypes "Int" (Int x)         = pure . const (Int x)
validateScalarTypes "Int" scalar          = Left . generateError (Scalar scalar) "Int"
validateScalarTypes "Boolean" (Boolean x) = pure . const (Boolean x)
validateScalarTypes "Boolean" scalar      = Left . generateError (Scalar scalar) "Boolean"
validateScalarTypes _ scalar              = pure . const scalar

validateList :: Bool -> (JSType -> InputValidation JSType) -> JSType -> [Prop] -> InputValidation JSType
validateList True validator' (JSList list') _ = JSList <$> mapM validator' list'
validateList True _ value' currentProp        = Left $ UnexpectedType currentProp "LIST" value'
validateList False validator' value' _        = validator' value'

validateEnumType :: Text -> [Text] -> JSType -> [Prop] -> InputValidation JSType
validateEnumType expected' tags jsType props = validateEnum (UnexpectedType props expected' jsType) tags jsType

validateLeaf :: Leaf -> JSType -> [Prop] -> InputValidation JSType
validateLeaf (LEnum tags core) jsType props      = validateEnumType (name core) tags jsType props
validateLeaf (LScalar core) (Scalar found) props = Scalar <$> validateScalarTypes (name core) found props
validateLeaf (LScalar core) jsType props         = Left $ generateError jsType (name core) props

validateInputObject :: [Prop] -> TypeLib -> GObject InputField -> (Text, JSType) -> InputValidation (Text, JSType)
validateInputObject prop' lib' (GObject parentFields _) (_name, JSObject fields) = do
  fieldTypeName' <- fieldType . unpackInputField <$> lookupField _name parentFields (UnknownField prop' _name)
  let currentProp = prop' ++ [Prop _name fieldTypeName']
  let error' = generateError (JSObject fields) fieldTypeName' currentProp
  inputObject' <- existsInputObjectType error' lib' fieldTypeName'
  mapM (validateInputObject currentProp lib' inputObject') fields >>= \x -> pure (_name, JSObject x)
validateInputObject prop' lib' (GObject parentFields _) (_name, jsType) = do
  field' <- unpackInputField <$> lookupField _name parentFields (UnknownField prop' _name)
  let fieldTypeName' = fieldType field'
  let currentProp = prop' ++ [Prop _name fieldTypeName']
  let error' = generateError jsType fieldTypeName' currentProp
  fieldType' <- existsLeafType error' lib' fieldTypeName'
  validateList (asList field') (\x -> validateLeaf fieldType' x currentProp) jsType currentProp >> pure (_name, jsType)

validateInput :: TypeLib -> InputType -> (Text, JSType) -> InputValidation JSType
validateInput typeLib (T.Object oType) (_, JSObject fields) =
  JSObject <$> mapM (validateInputObject [] typeLib oType) fields
validateInput _ (T.Object (GObject _ core)) (_, jsType) = Left $ generateError jsType (name core) []
validateInput _ (T.Scalar core) (_, jsValue) = validateLeaf (LScalar core) jsValue []
validateInput _ (T.Enum tags core) (_, jsValue) = validateLeaf (LEnum tags core) jsValue []

validateInputValue :: Bool -> TypeLib -> InputType -> (Text, JSType) -> InputValidation JSType
validateInputValue isList lib' iType' (key', value') =
  validateList isList (\x -> validateInput lib' iType' (key', x)) value' []
