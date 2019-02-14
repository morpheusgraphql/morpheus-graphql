{-# LANGUAGE TypeOperators , FlexibleInstances , ScopedTypeVariables #-}

module Data.Morpheus.Schema.InputValue
    (inputValueName)
where


import           Data.Text                      ( Text(..))
import           Data.Morpheus.Types.Types     ( (::->)(..))
import           Data.Morpheus.ErrorMessage    ( semanticError
                                                , handleError
                                                )
import           Data.Morpheus.Types.Introspection ( GQL__InputValue(..) )

inputValueName :: GQL__InputValue -> Text
inputValueName  = name