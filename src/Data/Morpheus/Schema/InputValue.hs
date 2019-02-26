{-# LANGUAGE TypeOperators , FlexibleInstances , ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Schema.InputValue
    (inputValueName, isRequired , inputValueMeta )
where


import           Data.Text                      ( Text(..))
import           Data.Morpheus.Types.Types     ( (::->)(..))
import           Data.Morpheus.Types.Introspection ( GQL__InputValue(..) )
import          Data.Morpheus.Types.MetaInfo (MetaInfo(..))

inputValueName :: GQL__InputValue -> Text
inputValueName  = name

isRequired :: GQL__InputValue -> Bool
isRequired x = defaultValue x /= "Nothing"

inputValueMeta :: GQL__InputValue -> MetaInfo
inputValueMeta input = MetaInfo {
         className = "TODO: Type"
         ,cons      = ""
         ,key       = name input
       }