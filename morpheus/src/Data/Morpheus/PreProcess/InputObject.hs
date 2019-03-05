{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.PreProcess.InputObject
    ( validateInputObject
    , validateInputVariable
    )
where


import           Data.Text                     as Text
                                                ( Text )
import           Data.Morpheus.Types.Introspection
                                                ( GQLTypeLib
                                                , GQL__Type
                                                )
import           Data.Morpheus.Types.Types      ( Validation(..) )
import           Data.Morpheus.Types.JSType     ( JSType(..) )
import           Data.Morpheus.PreProcess.Utils ( typeBy )
import           Data.Morpheus.ErrorMessage     ( fieldTypeMismatch )
import qualified Data.Morpheus.Schema.GQL__Type
                                               as T
import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..) )

typeMismatch :: MetaInfo -> JSType -> Text -> Validation JSType
typeMismatch _ (JSString x) "String"  = pure (JSString x)
typeMismatch _ (JSInt    x) "Int"     = pure (JSInt x)
typeMismatch _ (JSBool   x) "Boolean" = pure (JSBool x)
typeMismatch meta isType shouldType =
    Left $ fieldTypeMismatch meta isType shouldType

validateInputObject
    :: GQLTypeLib -> GQL__Type -> (Text, JSType) -> Validation (Text, JSType)
validateInputObject typeLib _parentType (_name, JSObject fields) = do
    _type   <- typeBy typeLib _parentType _name
    fields' <- mapM (validateInputObject typeLib _type) fields
    pure (_name, JSObject fields')

validateInputObject typeLib _parentType (_key, x) =
    typeBy typeLib _parentType _key >>= typeMismatch meta x . T.name >> pure
        (_key, x)
  where
    meta = MetaInfo { className = T.name _parentType, cons = "", key = _key }


validateInputVariable :: GQLTypeLib -> GQL__Type -> JSType -> Validation JSType
validateInputVariable typeLib _type (JSObject fields) =
    JSObject <$> mapM (validateInputObject typeLib _type) fields
