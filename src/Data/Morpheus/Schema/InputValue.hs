{-# LANGUAGE TypeOperators , FlexibleInstances , ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Schema.InputValue
    (inputValueName, isRequired )
where


import           Data.Text                      ( Text(..))
import           Data.Morpheus.Types.Types     ( (::->)(..))
import           Data.Morpheus.Types.Introspection ( GQL__InputValue(..) )

inputValueName :: GQL__InputValue -> Text
inputValueName  = name

isRequired :: GQL__InputValue -> Bool
isRequired x = defaultValue x /= "Nothing"