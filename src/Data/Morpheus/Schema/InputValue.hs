{-# LANGUAGE TypeOperators , FlexibleInstances , ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Schema.InputValue
  ( name
  , isRequired
  , inputValueMeta
  , typeName
  )
where

import           Data.Text                      ( Text(..) )
import           Data.Morpheus.Types.Types      ( (::->)(..) )
import           Data.Morpheus.Types.Introspection
                                                ( GQL__InputValue
                                                , GQL__Type
                                                )
import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..) )
import qualified Data.Morpheus.Schema.GQL__InputValue
                                               as I
                                                ( GQL__InputValue(..) )
import qualified Data.Morpheus.Schema.GQL__Type
                                               as T
                                                ( GQL__Type(..) )

name :: GQL__InputValue -> Text
name = I.name

isRequired :: GQL__InputValue -> Bool
isRequired x = I.defaultValue x /= "Nothing"

typeName :: GQL__InputValue -> Text
typeName x = case I._type x of
  Nothing -> "Error"
  Just t  -> T.name t

inputValueMeta :: GQL__InputValue -> MetaInfo
inputValueMeta input =
  MetaInfo { className = "TODO: Type", cons = "", key = I.name input }
