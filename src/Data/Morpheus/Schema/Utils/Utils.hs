{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Schema.Utils.Utils
  ( Type
  , Field
  , InputValue
  , createObjectType
  , typeFromObject
  , typeFromInputObject
  , typeFromLeaf
  , typeFromUnion
  ) where

import           Data.Morpheus.Schema.EnumValue     (EnumValue, createEnumValue)
import qualified Data.Morpheus.Schema.Field         as F (Field (..), createFieldWith)
import qualified Data.Morpheus.Schema.InputValue    as IN (InputValue (..), createInputValueWith)
import qualified Data.Morpheus.Schema.Internal.AST  as I (Core (..), Field (..), GObject (..), InputField (..),
                                                          InputObject, Leaf (..), ObjectField (..), OutputObject)
import           Data.Morpheus.Schema.Type          (Type (..))
import           Data.Morpheus.Schema.TypeKind      (TypeKind (..))
import           Data.Morpheus.Types.Describer      ((::->))
import           Data.Morpheus.Types.Query.Operator (ListWrapper (..))
import           Data.Text                          (Text)

type InputValue = IN.InputValue Type

type Field = F.Field Type

inputValueFromArg :: (Text, I.InputField) -> InputValue
inputValueFromArg (key', input') = IN.createInputValueWith key' (createInputObjectType input')

createInputObjectType :: I.InputField -> Type
createInputObjectType (I.InputField field') = wrap field' $ createType (I.kind field') (I.fieldType field') "" []

wrap :: I.Field -> Type -> Type
wrap field' = wrapNotNull field' . wrapList field'

wrapNotNull :: I.Field -> Type -> Type
wrapNotNull field' type' =
  if I.notNull field'
    then wrapAs NON_NULL type'
    else type'

wrapListSingle :: ListWrapper -> Type -> Type
wrapListSingle (ListWrapper True) type'  = wrapAs LIST (wrapAs NON_NULL type')
wrapListSingle (ListWrapper False) type' = wrapAs LIST type'

wrapListRec :: [ListWrapper] -> Type -> Type
wrapListRec [] type'     = type'
wrapListRec (x:xs) type' = wrapListRec xs (wrapListSingle x type')

wrapList :: I.Field -> Type -> Type
wrapList field' = wrapListRec (I.fieldTypeWrappers field')

fieldFromObjectField :: (Text, I.ObjectField) -> Field
fieldFromObjectField (key', field') =
  F.createFieldWith key' (wrap (I.fieldContent field') $ createType kind' getType "" []) args'
  where
    getType = I.fieldType $ I.fieldContent field'
    args' = map inputValueFromArg $ I.args field'
    kind' = I.kind $ I.fieldContent field'

typeFromLeaf :: (Text, I.Leaf) -> Type
typeFromLeaf (key', I.LScalar (I.Core _ desc'))     = createLeafType SCALAR key' desc' []
typeFromLeaf (key', I.LEnum tags' (I.Core _ desc')) = createLeafType ENUM key' desc' (map createEnumValue tags')

resolveNothing :: a ::-> Maybe b
resolveNothing = return Nothing

resolveMaybeList :: [b] -> a ::-> Maybe [b]
resolveMaybeList list' = return (Just list')

createLeafType :: TypeKind -> Text -> Text -> [EnumValue] -> Type
createLeafType kind' name' desc' enums' =
  Type
    { kind = kind'
    , name = Just name'
    , description = Just desc'
    , fields = resolveNothing
    , ofType = Nothing
    , interfaces = Nothing
    , possibleTypes = Nothing
    , enumValues = resolveMaybeList enums'
    , inputFields = Nothing
    }

typeFromUnion :: (Text, [I.Field]) -> Type
typeFromUnion (name', fields') =
  Type
    { kind = UNION
    , name = Just name'
    , description = Just "TODO"
    , fields = resolveNothing
    , ofType = Nothing
    , interfaces = Nothing
    , possibleTypes = Just (map (\x -> createObjectType (I.fieldType x) "" []) fields')
    , enumValues = return Nothing
    , inputFields = Nothing
    }

typeFromObject :: (Text, I.OutputObject) -> Type
typeFromObject (key', I.GObject fields' (I.Core _ description')) =
  createObjectType key' description' (map fieldFromObjectField fields')

typeFromInputObject :: (Text, I.InputObject) -> Type
typeFromInputObject (key', I.GObject fields' (I.Core _ description')) =
  createInputObject key' description' (map inputValueFromArg fields')

createObjectType :: Text -> Text -> [Field] -> Type
createObjectType = createType OBJECT

createInputObject :: Text -> Text -> [InputValue] -> Type
createInputObject name' desc' fields' =
  Type
    { kind = INPUT_OBJECT
    , name = Just name'
    , description = Just desc'
    , fields = resolveMaybeList []
    , ofType = Nothing
    , interfaces = Nothing
    , possibleTypes = Nothing
    , enumValues = resolveNothing
    , inputFields = Just fields'
    }

createType :: TypeKind -> Text -> Text -> [Field] -> Type
createType kind' name' desc' fields' =
  Type
    { kind = kind'
    , name = Just name'
    , description = Just desc'
    , fields = resolveMaybeList fields'
    , ofType = Nothing
    , interfaces = Just []
    , possibleTypes = Just []
    , enumValues = resolveMaybeList []
    , inputFields = Just []
    }

wrapAs :: TypeKind -> Type -> Type
wrapAs kind' contentType =
  Type
    { kind = kind'
    , name = Nothing
    , description = Nothing
    , fields = resolveNothing
    , ofType = Just contentType
    , interfaces = Nothing
    , possibleTypes = Nothing
    , enumValues = resolveNothing
    , inputFields = Nothing
    }
