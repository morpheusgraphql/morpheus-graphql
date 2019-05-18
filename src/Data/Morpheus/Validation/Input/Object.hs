{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Validation.Input.Object
  ( validateInputObject
  , validateInputValue
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

validateEnumType :: Text -> [Text] -> JSType -> [Prop] -> InputValidation JSType
validateEnumType expected' tags jsType props = validateEnum (UnexpectedType props expected' jsType) tags jsType

validateLeaf :: Leaf -> JSType -> [Prop] -> InputValidation JSType
validateLeaf (LEnum tags core) jsType props      = validateEnumType (name core) tags jsType props
validateLeaf (LScalar core) (Scalar found) props = Scalar <$> validateScalarTypes (name core) found props
validateLeaf (LScalar core) jsType props         = Left $ generateError jsType (name core) props

validateInputObject ::
     [Prop] -> TypeLib -> Int -> GObject InputField -> (Text, JSType) -> InputValidation (Text, JSType)
validateInputObject prop' lib' listDeep' (GObject parentFields pos) (_name, value') = do
  field' <- unpackInputField <$> lookupField _name parentFields (UnknownField prop' _name)
  case value' of
    JSList list'
      | listDeep' < length (fieldTypeWrappers field') ->
        mapM_ (recValidate (listDeep' + 1)) list' >> pure (_name, JSList list')
    JSList list' -> Left $ generateError (JSList list') (fieldType field') prop'
    JSObject fields -> do
      let fieldTypeName' = fieldType field'
      let currentProp = prop' ++ [Prop _name fieldTypeName']
      let error' = generateError (JSObject fields) fieldTypeName' currentProp
      inputObject' <- existsInputObjectType error' lib' fieldTypeName'
      if listDeep' > 0 && listDeep' < length (fieldTypeWrappers field')
        then Left $ UnexpectedType prop' (T.concat ["[", fieldTypeName', "]"]) (JSObject fields)
        else mapM (validateInputObject currentProp lib' 0 inputObject') fields >>= \x -> pure (_name, JSObject x)
    jsType -> do
      let fieldTypeName' = fieldType field'
      let currentProp = prop' ++ [Prop _name fieldTypeName']
      let error' = generateError jsType fieldTypeName' currentProp
      if listDeep' > 0 && listDeep' < length (fieldTypeWrappers field')
        then Left $ UnexpectedType prop' (T.concat ["[", fieldTypeName', "]"]) jsType
        else do
          fieldType' <- existsLeafType error' lib' fieldTypeName'
          validateLeaf fieldType' jsType currentProp >> pure (_name, jsType)
  where
    recValidate newDeep' x = validateInputObject prop' lib' newDeep' (GObject parentFields pos) (_name, x)

validateInput :: TypeLib -> InputType -> (Text, JSType) -> InputValidation JSType
validateInput typeLib (T.Object oType) (_, JSObject fields) =
  JSObject <$> mapM (validateInputObject [] typeLib 0 oType) fields
validateInput _ (T.Object (GObject _ core)) (_, jsType) = Left $ generateError jsType (name core) []
validateInput _ (T.Scalar core) (_, jsValue) = validateLeaf (LScalar core) jsValue []
validateInput _ (T.Enum tags core) (_, jsValue) = validateLeaf (LEnum tags core) jsValue []

validateInputValue :: [ListWrapper] -> TypeLib -> InputType -> (Text, JSType) -> InputValidation JSType
validateInputValue [] _ _ (_, list'@(JSList _)) = Left $ UnexpectedType [] "TODO:Not List" list'
validateInputValue (_:xs) lib' iType' (key', JSList list') =
  JSList <$> mapM (\x -> validateInputValue xs lib' iType' (key', x)) list'
validateInputValue [] lib' iType' value' = validateInput lib' iType' value'
validateInputValue _ _ _ (_, value') = Left $ UnexpectedType [] "TODO:LIST" value'
