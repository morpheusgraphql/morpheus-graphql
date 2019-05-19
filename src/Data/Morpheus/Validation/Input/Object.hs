{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Validation.Input.Object
  ( validateInputValue
  ) where

import           Data.Morpheus.Error.Input            (InputError (..), InputValidation, Prop (..))
import           Data.Morpheus.Schema.Internal.AST    (Core (..), Field (..), GObject (..), InputField (..),
                                                       InputObject, InputType, Leaf (..), TypeLib (..))
import qualified Data.Morpheus.Schema.Internal.AST    as T (InternalType (..))
import           Data.Morpheus.Types.JSType           (JSType (..), ScalarValue (..))
import           Data.Morpheus.Types.Query.Operator   (TypeWrapper (..))
import           Data.Morpheus.Validation.Input.Enum  (validateEnum)
import           Data.Morpheus.Validation.Utils.Utils (lookupField, lookupType)
import           Data.Text                            (Text)
import qualified Data.Text                            as T (concat)

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

validateLeaf :: Leaf -> JSType -> [Prop] -> InputValidation JSType
validateLeaf (LEnum tags core) jsType props      = validateEnum (UnexpectedType props (name core) jsType) tags jsType
validateLeaf (LScalar core) (Scalar found) props = Scalar <$> validateScalarTypes (name core) found props
validateLeaf (LScalar core) jsType props         = Left $ generateError jsType (name core) props

validateI :: [Prop] -> TypeLib -> GObject InputField -> (Text, JSType) -> InputValidation (Text, JSType)
validateI prop' lib' parent'@(GObject fields' _) (_name, value') = do
  wrappers' <- fieldTypeWrappers . unpackInputField <$> lookupField _name fields' (UnknownField prop' _name)
  validateInputObject prop' lib' wrappers' parent' (_name, value')

isNullableType :: [TypeWrapper] -> Bool
isNullableType (NonNullType:_) = False
isNullableType _               = True

unwrapped :: [TypeWrapper] -> Bool
unwrapped []            = True
unwrapped [NonNullType] = True
unwrapped _             = False

validateInputObject ::
     [Prop] -> TypeLib -> [TypeWrapper] -> GObject InputField -> (Text, JSType) -> InputValidation (Text, JSType)
validateInputObject prop' lib' (ListType:wrappers') (GObject parentFields pos) (_name, JSList list') =
  mapM_ recValidate list' >> pure (_name, JSList list')
  where
    recValidate x = validateInputObject prop' lib' wrappers' (GObject parentFields pos) (_name, x)
validateInputObject prop' lib' wrappers' (GObject parentFields _) (_name, value') = do
  field' <- getField
  case value' of
    JSNull
      | isNullableType wrappers' -> return (_name, value')
    JSObject fields
      | unwrapped wrappers' -> mapM_ recVal fields >> return (_name, value')
      where recVal v' = do
              (fieldTypeName', currentProp, error') <- validationData (JSObject fields)
              inputObject' <- existsInputObjectType error' lib' fieldTypeName'
              validateI currentProp lib' inputObject' v'
    leafValue'
      | unwrapped wrappers' -> do
        (fieldTypeName', currentProp, error') <- validationData leafValue'
        leafType' <- existsLeafType error' lib' fieldTypeName'
        validateLeaf leafType' leafValue' currentProp >> pure (_name, leafValue')
    invalidValue' -> Left $ unexpectedType wrappers' prop' (fieldType field') invalidValue'
  where
    validationData x = do
      fieldTypeName' <- fieldType <$> getField
      let currentProp = prop' ++ [Prop _name fieldTypeName']
      let inputError = generateError x fieldTypeName' currentProp
      return (fieldTypeName', currentProp, inputError)
    getField = unpackInputField <$> lookupField _name parentFields (UnknownField prop' _name)

showTypeSignature :: [TypeWrapper] -> Text -> Text
showTypeSignature [] type'               = type'
showTypeSignature (ListType:xs) type'    = showTypeSignature xs $ T.concat ["[", type', "]"]
showTypeSignature (NonNullType:xs) type' = showTypeSignature xs $ T.concat [type', "!"]

unexpectedType :: [TypeWrapper] -> [Prop] -> Text -> JSType -> InputError
unexpectedType wrappers' prop' type' = UnexpectedType prop' (showTypeSignature wrappers' type')

validateInput :: TypeLib -> InputType -> (Text, JSType) -> InputValidation JSType
validateInput typeLib (T.Object oType) (_, JSObject fields) = JSObject <$> mapM (validateI [] typeLib oType) fields
validateInput _ (T.Object (GObject _ core)) (_, jsType)     = Left $ generateError jsType (name core) []
validateInput _ (T.Scalar core) (_, jsValue)                = validateLeaf (LScalar core) jsValue []
validateInput _ (T.Enum tags core) (_, jsValue)             = validateLeaf (LEnum tags core) jsValue []

validateInputValue :: [TypeWrapper] -> TypeLib -> InputType -> (Text, JSType) -> InputValidation JSType
validateInputValue w'@(NonNullType:_) _ _ (_, JSNull) = Left $ unexpectedType w' [] "TODO:Type" JSNull
validateInputValue _ _ _ (_, JSNull) = return JSNull
validateInputValue (NonNullType:wrappers') lib' type' value' = validateInputValue wrappers' lib' type' value'
validateInputValue (ListType:xs) lib' iType' (key', JSList list') = JSList <$> mapM listCheck list'
  where
    listCheck element' = validateInputValue xs lib' iType' (key', element')
validateInputValue [] lib' iType' value' = validateInput lib' iType' value'
validateInputValue wrappers' _ _ (_, value') = Left $ unexpectedType wrappers' [] "TODO:Type" value'
