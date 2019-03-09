{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.PreProcess.Utils
    ( existsType
    , typeBy
    , fieldOf
    )
where

import qualified Data.Map                      as M
                                                ( lookup )
import           Data.Text                     as TX
                                                ( Text
                                                , concat
                                                )
import           Data.Morpheus.Types.Types      ( Validation )
import           Data.Morpheus.Types.Introspection
                                                ( GQLTypeLib
                                                , GQL__Type
                                                , GQL__Field
                                                )
import           Data.Morpheus.ErrorMessage     ( handleError
                                                , cannotQueryField
                                                )
import qualified Data.Morpheus.Schema.GQL__Type
                                               as T
import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..), Position(..), LineMarks )
import           Data.Morpheus.Schema.SchemaField
                                                ( getFieldTypeByKey
                                                , selectFieldByKey
                                                )

fieldTypeOf :: LineMarks -> GQL__Type -> Text -> Validation GQL__Type
fieldTypeOf lines _type fieldName = case getFieldTypeByKey fieldName _type of
    Nothing -> Left $ cannotQueryField lines meta
    Just fieldType -> pure fieldType
    where 
        meta = MetaInfo
            { key      = fieldName
            , typeName = T.name _type
            , position = Position 0
            }


typeBy lines typeLib _parentType _name = fieldTypeOf lines _parentType _name >>= fieldType
    where fieldType field = existsType (T.name field) typeLib


existsType :: TX.Text -> GQLTypeLib -> Validation GQL__Type
existsType typeName typeLib = case M.lookup typeName typeLib of
    Nothing -> handleError $ TX.concat ["type does not exist", typeName]
    Just x  -> pure x

fieldOf :: LineMarks -> GQL__Type -> Text -> Validation GQL__Field
fieldOf lines _type fieldName = case selectFieldByKey fieldName _type of
    Nothing -> Left $ cannotQueryField lines meta
    Just field -> pure field
    where  
        meta = MetaInfo
            {   key = fieldName
                , typeName = T.name _type
                , position = Position 0
            }

