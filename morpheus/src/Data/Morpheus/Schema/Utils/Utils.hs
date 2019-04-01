{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Schema.Utils.Utils
  ( Type
  , Field
  , InputValue
  , createObjectType
  , wrapListType
  , typeFromObject
  ) where

import qualified Data.Morpheus.Schema.Field          as F (Field (..), createFieldWith)
import qualified Data.Morpheus.Schema.InputValue     as IN (InputValue (..))
import qualified Data.Morpheus.Schema.Internal.Types as I (Core (..), Field (..), GObject (..), ObjectField (..),
                                                           OutputObject)
import           Data.Morpheus.Schema.Type           (Type (..))
import           Data.Morpheus.Schema.TypeKind       (TypeKind (..))
import           Data.Morpheus.Types.Describer       (EnumOf (..), WithDeprecationArgs (..))
import           Data.Text                           (Text)

type InputValue = IN.InputValue Type

type Field = F.Field Type

fieldFromObjectField :: (Text, I.ObjectField) -> Field
fieldFromObjectField (key', field') = F.createFieldWith key' (createObjectType getType "" []) [] -- I.args --
  where
    getType = I.fieldType $ I.fieldContent field'

typeFromObject :: (Text, I.OutputObject) -> Type
typeFromObject (key', I.GObject fields' (I.Core _ description')) =
  Type
    { kind = EnumOf OBJECT
    , name = key'
    , description = description'
    , fields = WithDeprecationArgs $ map fieldFromObjectField fields'
    , ofType = Nothing
    , interfaces = []
    , possibleTypes = []
    , enumValues = WithDeprecationArgs []
    , inputFields = []
    }

createObjectType :: Text -> Text -> [Field] -> Type
createObjectType tName desc tFields =
  Type
    { kind = EnumOf OBJECT
    , name = tName
    , description = desc
    , fields = WithDeprecationArgs tFields
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
