{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Schema.Utils.Utils
  ( Type
  , Field
  , InputValue
  , createObjectType
  , wrapListType
  , typeFromObject
  , typeFromInputObject
  ) where

import qualified Data.Morpheus.Schema.Field          as F (Field (..), createFieldWith)
import qualified Data.Morpheus.Schema.InputValue     as IN (InputValue (..), createInputValueWith)
import qualified Data.Morpheus.Schema.Internal.Types as I (Core (..), Field (..), GObject (..), InputField (..),
                                                           InputObject, ObjectField (..), OutputObject)
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

fieldFromObjectField :: (Text, I.ObjectField) -> Field
fieldFromObjectField (key', field') = F.createFieldWith key' (createObjectType getType "" []) args'
  where
    getType = I.fieldType $ I.fieldContent field'
    args' = map inputValueFromArg $ I.args field'

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
    , name = name'
    , description = desc'
    , fields = WithDeprecationArgs []
    , ofType = Nothing
    , interfaces = []
    , possibleTypes = []
    , enumValues = WithDeprecationArgs []
    , inputFields = fields'
    }

createType :: TypeKind -> Text -> Text -> [Field] -> Type
createType kind' name' desc' fields' =
  Type
    { kind = EnumOf kind'
    , name = name'
    , description = desc'
    , fields = WithDeprecationArgs fields'
    , ofType = Nothing
    , interfaces = []
    , possibleTypes = []
    , enumValues = WithDeprecationArgs []
    , inputFields = []
    }

wrapListType :: Type -> Type
wrapListType contentType =
  Type
    { kind = EnumOf LIST
    , name = ""
    , description = ""
    , fields = WithDeprecationArgs []
    , ofType = Just contentType
    , interfaces = []
    , possibleTypes = []
    , enumValues = WithDeprecationArgs []
    , inputFields = []
    }
