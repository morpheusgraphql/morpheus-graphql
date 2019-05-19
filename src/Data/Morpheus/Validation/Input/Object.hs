{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Validation.Input.Object
  ( validateInputValue
  ) where

import           Data.Morpheus.Error.Input            (InputError (..), InputValidation, Prop (..))
import           Data.Morpheus.Schema.Internal.AST    (Core (..), Field (..), GObject (..), InputField (..),
                                                       InputObject, InputType, Leaf (..), TypeLib (..))
import qualified Data.Morpheus.Schema.Internal.AST    as T (InternalType (..))
import           Data.Morpheus.Types.JSType           (JSType (..), ScalarValue (..))
import           Data.Morpheus.Types.Query.Operator   (ListWrapper (..))
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

validateInputObject ::
     [Prop] -> TypeLib -> [ListWrapper] -> GObject InputField -> (Text, JSType) -> InputValidation (Text, JSType)
validateInputObject prop' lib' wrappers' (GObject parentFields pos) (_name, value') = do
  field' <- getField
  case value' of
    JSList list'
      | isWrappedInList -> mapM_ (recValidate (tail wrappers')) list' >> pure (_name, JSList list')
    JSObject fields
      | not isWrappedInList -> mapM recVal fields >>= \x -> pure (_name, JSObject x)
      where recVal v' = do
              (fieldTypeName', currentProp, error') <- validationData (JSObject fields)
              inputObject' <- existsInputObjectType error' lib' fieldTypeName'
              validateI currentProp lib' inputObject' v'
    leafValue'
      | not isWrappedInList -> do
        (fieldTypeName', currentProp, error') <- validationData leafValue'
        leafType' <- existsLeafType error' lib' fieldTypeName'
        validateLeaf leafType' leafValue' currentProp >> pure (_name, leafValue')
    invalidValue' -> Left $ UnexpectedType prop' (T.concat ["[", fieldType field', "]"]) invalidValue'
  where
    isWrappedInList = 0 < length wrappers'
    validationData x = do
      fieldTypeName' <- fieldType <$> getField
      let currentProp = prop' ++ [Prop _name fieldTypeName']
      let inputError = generateError x fieldTypeName' currentProp
      return (fieldTypeName', currentProp, inputError)
    getField = unpackInputField <$> lookupField _name parentFields (UnknownField prop' _name)
    recValidate _ JSNull
      | hasListNullableElements wrappers' = pure (_name, JSNull)
    recValidate list' x = validateInputObject prop' lib' list' (GObject parentFields pos) (_name, x)

validateInput :: TypeLib -> InputType -> (Text, JSType) -> InputValidation JSType
validateInput typeLib (T.Object oType) (_, JSObject fields) = JSObject <$> mapM (validateI [] typeLib oType) fields
validateInput _ (T.Object (GObject _ core)) (_, jsType)     = Left $ generateError jsType (name core) []
validateInput _ (T.Scalar core) (_, jsValue)                = validateLeaf (LScalar core) jsValue []
validateInput _ (T.Enum tags core) (_, jsValue)             = validateLeaf (LEnum tags core) jsValue []

hasListNullableElements :: [ListWrapper] -> Bool
hasListNullableElements (ListWrapper True:_) = False
hasListNullableElements _                    = True

validateInputValue :: [ListWrapper] -> TypeLib -> InputType -> (Text, JSType) -> InputValidation JSType
validateInputValue [] _ _ (_, list'@(JSList _)) = Left $ UnexpectedType [] "TODO:Not List" list'
validateInputValue w@(_:xs) lib' iType' (key', JSList list') = JSList <$> mapM listCheck list'
  where
    listCheck JSNull
      | hasListNullableElements w = return JSNull
    listCheck element' = validateInputValue xs lib' iType' (key', element')
validateInputValue [] lib' iType' value' = validateInput lib' iType' value'
validateInputValue _ _ _ (_, value') = Left $ UnexpectedType [] "TODO:LIST" value'
