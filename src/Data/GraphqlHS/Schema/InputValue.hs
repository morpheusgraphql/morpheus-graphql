{-# LANGUAGE TypeOperators , FlexibleInstances , ScopedTypeVariables #-}

module Data.GraphqlHS.Schema.InputValue
    (inputValueName)
where


import           Data.Text                      ( Text(..))
import           Data.GraphqlHS.Types.Types     ( (::->)(..))
import           Data.GraphqlHS.ErrorMessage    ( semanticError
                                                , handleError
                                                )
import           Data.GraphqlHS.Types.Introspection ( GQL__InputValue(..) )

inputValueName :: GQL__InputValue -> Text
inputValueName  = name