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
  ) where

import           Data.Morpheus.Schema.EnumValue      (EnumValue, createEnumValue)
import qualified Data.Morpheus.Schema.Field          as F (Field (..), createFieldWith)
import qualified Data.Morpheus.Schema.InputValue     as IN (InputValue (..), createInputValueWith)
import qualified Data.Morpheus.Schema.Internal.Types as I (Core (..), Field (..), GObject (..), InputField (..),
                                                           InputObject, Leaf (..), ObjectField (..), OutputObject)
import           Data.Morpheus.Schema.Type           (Type (..))
import           Data.Morpheus.Schema.TypeKind       (TypeKind (..))
import           Data.Morpheus.Types.Describer       (EnumOf (..), WithDeprecationArgs (..))
import           Data.Text                           (Text)

type InputValue = IN.InputValue Type

type Field = F.Field Type

inputValueFromArg :: (Text, I.InputField) -> InputValue
inputValueFromArg (key', input') = IN.createInputValueWith key' (createInputObjectType input')

createInputObjectType :: I.InputField -> Type
createInputObjectType (I.InputField field') = createType (I.kind field') (I.fieldType field') "" []

wrapNotNull :: I.ObjectField -> Type -> Type
wrapNotNull field' type' =
  if I.notNull $ I.fieldContent field'
    then wrapAs NON_NULL type'
    else type'

wrapList :: I.ObjectField -> Type -> Type
wrapList field' type' =
  if I.asList $ I.fieldContent field'
    then wrapAs LIST type'
    else type'

fieldFromObjectField :: (Text, I.ObjectField) -> Field
fieldFromObjectField (key', field') =
  F.createFieldWith key' (wrapNotNull field' $ wrapList field' $ createObjectType getType "" []) args'
  where
    getType = I.fieldType $ I.fieldContent field'
    args' = map inputValueFromArg $ I.args field'

typeFromLeaf :: (Text, I.Leaf) -> Type
typeFromLeaf (key', I.LScalar (I.Core _ desc'))     = createLeafType SCALAR key' desc' []
typeFromLeaf (key', I.LEnum tags' (I.Core _ desc')) = createLeafType ENUM key' desc' (map createEnumValue tags')

createLeafType :: TypeKind -> Text -> Text -> [EnumValue] -> Type
createLeafType kind' name' desc' enums' =
  Type
    { kind = EnumOf kind'
    , name = Just name'
    , description = Just desc'
    , fields = Nothing
    , ofType = Nothing
    , interfaces = Nothing
    , possibleTypes = Nothing
    , enumValues = Just $ WithDeprecationArgs enums'
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
    { kind = EnumOf INPUT_OBJECT
    , name = Just name'
    , description = Just desc'
    , fields = Just $ WithDeprecationArgs []
    , ofType = Nothing
    , interfaces = Nothing
    , possibleTypes = Nothing
    , enumValues = Nothing
    , inputFields = Just fields'
    }

createType :: TypeKind -> Text -> Text -> [Field] -> Type
createType kind' name' desc' fields' =
  Type
    { kind = EnumOf kind'
    , name = Just name'
    , description = Just desc'
    , fields = Just $ WithDeprecationArgs fields'
    , ofType = Nothing
    , interfaces = Just []
    , possibleTypes = Just []
    , enumValues = Just $ WithDeprecationArgs []
    , inputFields = Just []
    }

wrapAs :: TypeKind -> Type -> Type
wrapAs kind' contentType =
  Type
    { kind = EnumOf kind'
    , name = Nothing
    , description = Nothing
    , fields = Nothing
    , ofType = Just contentType
    , interfaces = Nothing
    , possibleTypes = Nothing
    , enumValues = Nothing
    , inputFields = Nothing
    }
