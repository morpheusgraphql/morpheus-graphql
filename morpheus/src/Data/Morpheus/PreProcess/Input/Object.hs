{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Input.Object
  ( validateInputObject
  , validateInput
  ) where

import           Data.Morpheus.Error.Input           (InputError (..), InputErrorKind (..), InputValidation, Prop (..),
                                                      typeMismatchMetaError)
import           Data.Morpheus.PreProcess.Input.Enum (validateEnum)
import           Data.Morpheus.PreProcess.Utils      (fieldOf, lookupType)
import           Data.Morpheus.Schema.Internal.Types (Core (..), Field (..), GObject (..), InputField (..), InputObject,
                                                      InputType, Leaf (..), TypeLib (..))
import qualified Data.Morpheus.Schema.Internal.Types as T (InternalType (..))
import           Data.Morpheus.Types.JSType          (JSType (..), ScalarValue (..))
import           Data.Text                           (Text)

-- import           Data.Morpheus.Types.MetaInfo        (Position)
-- import qualified Data.Text                           as T (intercalate)
generateError :: JSType -> [Prop] -> InputError
generateError jsType path' = InputError {path = path', errorKind = UnexpectedType jsType}

existsInputObjectType :: InputError -> TypeLib -> Text -> InputValidation InputObject
existsInputObjectType error' lib' = lookupType error' (inputObject lib')

existsLeafType :: InputError -> TypeLib -> Text -> InputValidation Leaf
existsLeafType error' lib' = lookupType error' (leaf lib')

-- convertError :: Position -> Text -> InputValidation a -> MetaValidation a
-- convertError position' type' (Left inpError) =
--  case errorKind inpError of
--    UnexpectedType jsType -> Left $ TypeMismatch meta jsType
--    UndefinedField        -> Left $ UnknownField meta
--  where
--    meta = MetaInfo {position = position', typeName = type', key = key'}
--    key' = T.intercalate "." $ fmap propKey (path inpError)
-- convertError _ _ (Right x) = pure x
validateScalarTypes :: Text -> ScalarValue -> [Prop] -> InputValidation ScalarValue
validateScalarTypes "String" (String x)   = pure . const (String x)
validateScalarTypes "String" scalar       = Left . generateError (Scalar scalar)
validateScalarTypes "Int" (Int x)         = pure . const (Int x)
validateScalarTypes "Int" scalar          = Left . generateError (Scalar scalar)
validateScalarTypes "Boolean" (Boolean x) = pure . const (Boolean x)
validateScalarTypes "Boolean" scalar      = Left . generateError (Scalar scalar)
validateScalarTypes _ scalar              = pure . const scalar

validateEnumType :: [Text] -> JSType -> [Prop] -> InputValidation JSType
validateEnumType tags jsType props = validateEnum error' tags jsType
  where
    error' = generateError jsType props

validateLeaf :: Leaf -> JSType -> [Prop] -> InputValidation JSType
validateLeaf (LScalar core) (Scalar found) props = Scalar <$> validateScalarTypes (name core) found props
validateLeaf (LEnum tags _) jsType props         = validateEnumType tags jsType props
validateLeaf _ jsType props                      = Left $ generateError jsType props

validateInputObject :: [Prop] -> TypeLib -> GObject InputField -> (Text, JSType) -> InputValidation (Text, JSType)
validateInputObject prop' lib' (GObject parentFields _) (_name, JSObject fields) = do
  fieldTypeName' <- fieldType . unpackInputField <$> fieldOf (pos, _name) parentFields _name
  let currentProp = prop' ++ [Prop _name fieldTypeName']
  let error' = generateError (JSObject fields) currentProp
  inputObject' <- existsInputObjectType error' lib' fieldTypeName'
  mapM (validateInputObject currentProp lib' inputObject') fields >>= \x -> pure (_name, JSObject x)
validateInputObject prop' lib' (GObject parentFields _) (_name, jsType) = do
  fieldTypeName' <- fieldType . unpackInputField <$> fieldOf (pos, _name) parentFields _name
  let currentProp = prop' ++ [Prop _name fieldTypeName']
  let error' = generateError jsType currentProp
  fieldType' <- existsLeafType error' lib' fieldTypeName'
  validateLeaf fieldType' jsType currentProp >> pure (_name, jsType)

validateInput :: TypeLib -> InputType -> (Text, JSType) -> InputValidation JSType
validateInput typeLib (T.Object oType) (key', JSObject fields) =
  JSObject <$> mapM (validateInputObject [Prop key' "TODO:"] typeLib oType pos) fields
validateInput _ (T.Object (GObject _ core)) (_, jsType) = typeMismatchMetaError pos (name core) jsType
validateInput _ (T.Scalar core) (varName, jsValue) = validateLeaf (LScalar core) jsValue [Prop varName (name core)]
validateInput _ (T.Enum tags core) (varName, jsValue) =
  validateLeaf (LEnum tags core) jsValue [Prop varName (name core)]
