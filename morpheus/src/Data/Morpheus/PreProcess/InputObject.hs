module Data.Morpheus.PreProcess.InputObject
    ( validateInputObject
    , validateInputVariable
    )
where


import           Data.Text                      ( Text )
import           Data.Morpheus.Types.Introspection
                                                ( GQLTypeLib
                                                , GQL__Type
                                                )
import           Data.Morpheus.Types.Types      ( Validation(..) )
import           Data.Morpheus.Types.JSType     ( JSType(..) )
import           Data.Morpheus.PreProcess.Utils ( typeBy )


validateInputObject
    :: GQLTypeLib -> GQL__Type -> (Text, JSType) -> Validation (Text, JSType)
validateInputObject typeLib _parentType (_name, JSObject fields) = do
    _type   <- typeBy typeLib _parentType _name
    fields' <- mapM (validateInputObject typeLib _type) fields
    pure (_name, JSObject fields')

validateInputObject typeLib _parentType (_name, x) =
    typeBy typeLib _parentType _name >> pure (_name, x)


validateInputVariable :: GQLTypeLib -> GQL__Type -> JSType -> Validation JSType
validateInputVariable typeLib _type (JSObject fields) =
    JSObject <$> mapM (validateInputObject typeLib _type) fields
