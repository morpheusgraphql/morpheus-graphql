{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.PreProcess.InputObject
  ( validateInputObject
  , validateInputVariable
  ) where

import           Data.Morpheus.PreProcess.Utils    (typeBy')
import qualified Data.Morpheus.Schema.GQL__Type    as T
import           Data.Morpheus.Types.Error         (InputError (..), InputValidation)
import           Data.Morpheus.Types.Introspection (GQLTypeLib, GQL__Type)
import           Data.Morpheus.Types.JSType        (JSType (..))
import           Data.Morpheus.Types.MetaInfo      (MetaInfo (..))
import           Data.Text                         as Text (Text, pack)

typeMismatch :: MetaInfo -> JSType -> Text -> InputValidation JSType
typeMismatch _ (JSString x) "String" = pure (JSString x)
typeMismatch _ (JSInt x) "Int"       = pure (JSInt x)
typeMismatch _ (JSBool x) "Boolean"  = pure (JSBool x)
typeMismatch meta isType sType       = Left $ TypeMismatch meta (Text.pack $ show isType) sType

validateFieldType :: MetaInfo -> JSType -> GQL__Type -> InputValidation JSType
validateFieldType meta x = typeMismatch meta x . T.name

validateInputObject :: GQLTypeLib -> GQL__Type -> (Text, JSType) -> InputValidation (Text, JSType)
validateInputObject typeLib _parentType (_name, JSObject fields) = do
  _type <- typeBy' 0 typeLib _parentType _name
  fields' <- mapM (validateInputObject typeLib _type) fields
  pure (_name, JSObject fields')
validateInputObject typeLib _parentType (_key, x) =
  typeBy' 0 typeLib _parentType _key >>= validateFieldType meta x >> pure (_key, x)
  where
    meta = MetaInfo {typeName = T.name _parentType, key = _key, position = 0}

validateInputVariable :: GQLTypeLib -> GQL__Type -> (Text, JSType) -> InputValidation JSType
validateInputVariable typeLib _type (_, JSObject fields) = JSObject <$> mapM (validateInputObject typeLib _type) fields
validateInputVariable _ _type (varName, x) = validateFieldType meta x _type
  where
    meta = MetaInfo {typeName = "", key = varName, position = 0}
