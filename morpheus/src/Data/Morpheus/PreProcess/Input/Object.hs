{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.Input.Object
  ( validateInputObject
  , validateInputVariable
  ) where

import           Data.Morpheus.PreProcess.Utils    (typeBy)
import qualified Data.Morpheus.Schema.GQL__Type    as T
import           Data.Morpheus.Types.Error         (MetaError (..), MetaValidation)
import           Data.Morpheus.Types.Introspection (GQLTypeLib, GQL__Type)
import           Data.Morpheus.Types.JSType        (JSType (..))
import           Data.Morpheus.Types.MetaInfo      (MetaInfo (..))
import           Data.Text                         as Text (Text, pack)

typeMismatch :: MetaInfo -> JSType -> Text -> MetaValidation JSType
typeMismatch _ (JSString x) "String" = pure (JSString x)
typeMismatch _ (JSInt x) "Int"       = pure (JSInt x)
typeMismatch _ (JSBool x) "Boolean"  = pure (JSBool x)
typeMismatch meta isType sType       = Left $ TypeMismatch meta (Text.pack $ show isType) sType

validateFieldType :: MetaInfo -> JSType -> GQL__Type -> MetaValidation JSType
validateFieldType meta x = typeMismatch meta x . T.name

validateInputObject :: GQLTypeLib -> GQL__Type -> (Text, JSType) -> MetaValidation (Text, JSType)
validateInputObject typeLib _parentType (_name, JSObject fields) = do
  typeSD <- typeBy 0 typeLib _parentType _name
  fieldsQS <- mapM (validateInputObject typeLib typeSD) fields
  pure (_name, JSObject fieldsQS)
validateInputObject typeLib _parentType (_key, x) =
  typeBy 0 typeLib _parentType _key >>= validateFieldType meta x >> pure (_key, x)
  where
    meta = MetaInfo {typeName = T.name _parentType, key = _key, position = 0}

validateInputVariable :: GQLTypeLib -> GQL__Type -> (Text, JSType) -> MetaValidation JSType
validateInputVariable typeLib _type (_, JSObject fields) = JSObject <$> mapM (validateInputObject typeLib _type) fields
validateInputVariable _ _type (varName, x) = validateFieldType meta x _type
  where
    meta = MetaInfo {typeName = "", key = varName, position = 0}
