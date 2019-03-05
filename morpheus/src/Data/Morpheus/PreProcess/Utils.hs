{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.PreProcess.Utils
    ( existsType,
    typeBy
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
                                                )
import           Data.Morpheus.ErrorMessage     ( handleError
                                                , cannotQueryField
                                                )
import qualified Data.Morpheus.Schema.GQL__Type
                                               as T
import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..) )
import           Data.Morpheus.Schema.SchemaField
                                                ( getFieldTypeByKey )

fieldTypeOf :: GQL__Type -> Text -> Validation GQL__Type
fieldTypeOf _type fieldName = case getFieldTypeByKey fieldName _type of
    Nothing -> Left $ cannotQueryField $ MetaInfo
        { key       = fieldName
        , cons      = ""
        , className = T.name _type
        }
    Just fieldType -> pure fieldType


typeBy typeLib _parentType _name = fieldTypeOf _parentType _name >>= fieldType
    where fieldType field = existsType (T.name field) typeLib


existsType :: TX.Text -> GQLTypeLib -> Validation GQL__Type
existsType typeName typeLib = case M.lookup typeName typeLib of
    Nothing -> handleError $ TX.concat ["type does not exist", typeName]
    Just x  -> pure x
