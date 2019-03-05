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
import           Data.Morpheus.ErrorMessage     ( handleError
                                                , cannotQueryField
                                                )
import qualified Data.Morpheus.Schema.GQL__Type
                                               as T
import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..) )

typeMismatch :: JSType -> Text -> MetaInfo -> Validation JSType
typeMismatch (JSString x) "String"  _    = pure (JSString x)
typeMismatch (JSInt    x) "Int"     _    = pure (JSInt x)
typeMismatch (JSBool   x) "Boolean" _    = pure (JSBool x)
typeMismatch _            _         meta = handleError "typemismatch"

validateInputObject
    :: GQLTypeLib -> GQL__Type -> (Text, JSType) -> Validation (Text, JSType)
validateInputObject typeLib _parentType (_name, JSObject fields) = do
    _type   <- typeBy typeLib _parentType _name
    fields' <- mapM (validateInputObject typeLib _type) fields
    pure (_name, JSObject fields')

validateInputObject typeLib _parentType (_name, x) =
    typeBy typeLib _parentType _name >>= typeMismatch x . T.name >> pure
        (_name, x)


validateInputVariable :: GQLTypeLib -> GQL__Type -> JSType -> Validation JSType
validateInputVariable typeLib _type (JSObject fields) =
    JSObject <$> mapM (validateInputObject typeLib _type) fields
