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

generateError :: [Prop] -> JSType -> InputError
generateError path' jsType = InputError {path = path', errorKind = UnexpectedType jsType}

existsInputObjectType :: InputError -> TypeLib -> Text -> InputValidation InputObject
existsInputObjectType error' lib' = lookupType error' (inputObject lib')

existsLeafType :: InputError -> TypeLib -> Text -> InputValidation Leaf
existsLeafType error' lib' = lookupType error' (leaf lib')

convertError :: Position -> Text -> InputValidation a -> MetaValidation a
convertError position' type' (Left inpError) =
  case errorKind inpError of
    UnexpectedType jsType -> Left $ TypeMismatch meta jsType
    UndefinedField        -> Left $ UnknownField meta
  where
    meta = MetaInfo {position = position', typeName = type', key = key'}
    key' = T.intercalate "." $ fmap propKey (path inpError)
convertError _ _ (Right x) = pure x

validateScalarTypes :: Text -> ScalarValue -> [Prop] -> InputValidation ScalarValue
validateScalarTypes "String" (String x)   = pure . const (String x)
validateScalarTypes "String" scalar       = Left . (`generateError` Scalar scalar)
validateScalarTypes "Int" (Int x)         = pure . const (Int x)
validateScalarTypes "Int" scalar          = Left . (`generateError` Scalar scalar)
validateScalarTypes "Boolean" (Boolean x) = pure . const (Boolean x)
validateScalarTypes "Boolean" scalar      = Left . (`generateError` Scalar scalar)
validateScalarTypes _ scalar              = pure . const scalar

validateFieldType :: InputType -> JSType -> [Prop] -> InputValidation JSType
validateFieldType (T.Scalar core) (Scalar found) props = Scalar <$> validateScalarTypes (name core) found props
validateFieldType (T.Object _) (JSObject x) _          = pure (JSObject x) -- TODO Validate Scalar
validateFieldType (T.Enum _ _) (JSEnum x) _            = pure (JSEnum x) -- TODO Validate Scalar
validateFieldType _ jsType props                       = Left $ generateError props jsType

validateInputObject ::
     [Prop] -> TypeLib -> GObject InputField -> Position -> (Text, JSType) -> MetaValidation (Text, JSType)
validateInputObject prop' lib' (GObject parentFields _) pos (_name, JSObject fields) = do
  fieldTypeName' <- fieldType . unpackInputField <$> fieldOf (pos, _name) parentFields _name
  let currentProp = prop' ++ [Prop _name fieldTypeName']
  let error' = generateError currentProp (JSObject fields)
  let toError = convertError pos fieldTypeName'
  inputObject' <- toError (existsInputObjectType error' lib' fieldTypeName')
  mapM (validateInputObject currentProp lib' inputObject' pos) fields >>= \x -> pure (_name, JSObject x)
validateInputObject prop' lib' (GObject parentFields _) pos (_name, jsType) = do
  fieldTypeName' <- fieldType . unpackInputField <$> fieldOf (pos, _name) parentFields _name
  let currentProp = prop' ++ [Prop _name fieldTypeName']
  let error' = generateError currentProp jsType
  let toError = convertError pos fieldTypeName'
  fieldType' <- toError (leafToInputType <$> existsLeafType error' lib' fieldTypeName')
  toError $ validateFieldType fieldType' jsType currentProp >> pure (_name, jsType)

validateInput :: TypeLib -> InputType -> Position -> (Text, JSType) -> MetaValidation JSType
validateInput typeLib (T.Object oType) pos (key', JSObject fields) =
  JSObject <$> mapM (validateInputObject [Prop key' "TODO:"] typeLib oType pos) fields
validateInput _ (T.Object (GObject _ core)) pos (_, jsType) = typeMismatchMetaError pos (name core) jsType
validateInput _ (T.Scalar core) pos (varName, jsValue) =
  convertError pos (name core) $ validateFieldType (T.Scalar core) jsValue [Prop varName (name core)]
validateInput _ (T.Enum _ core) pos (varName, jsValue) =
  convertError pos (name core) $ validateFieldType (T.Scalar core) jsValue [Prop varName (name core)]
