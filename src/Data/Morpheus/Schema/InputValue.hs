{-# LANGUAGE TypeOperators , FlexibleInstances , ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Schema.InputValue
    (name, isRequired , inputValueMeta , typeName )
where


import             Data.Text                      ( Text(..))
import             Data.Morpheus.Types.Types     ( (::->)(..))
import  qualified  Data.Morpheus.Types.Introspection as T ( GQL__InputValue , GQL__Type(..) )
import             Data.Morpheus.Types.MetaInfo (MetaInfo(..))
import qualified Data.Morpheus.Schema.GQL__InputValue as I (GQL__InputValue(..))

name :: T.GQL__InputValue -> Text
name  = I.name

isRequired :: T.GQL__InputValue -> Bool
isRequired x = I.defaultValue x /= "Nothing"

typeName :: T.GQL__InputValue -> Text
typeName x = case I._type x of
  Nothing -> "Error"
  Just t -> T.name t

inputValueMeta :: T.GQL__InputValue -> MetaInfo
inputValueMeta input = MetaInfo {
         className = "TODO: Type"
         ,cons      = ""
         ,key       = I.name input
       }