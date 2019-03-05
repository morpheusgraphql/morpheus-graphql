{-# LANGUAGE OverloadedStrings  #-}

module Data.Morpheus.PreProcess.Utils
    ( existsType
    )
where

import qualified Data.Map                      as M
                                                ( lookup )
import qualified Data.Text                     as T
                                                ( Text
                                                , concat
                                                )
import           Data.Morpheus.Types.Types      ( Validation )
import           Data.Morpheus.Types.Introspection
                                                ( GQLTypeLib
                                                , GQL__Type
                                                )
import           Data.Morpheus.ErrorMessage     ( handleError )


existsType :: T.Text -> GQLTypeLib -> Validation GQL__Type
existsType typeName typeLib = case M.lookup typeName typeLib of
    Nothing -> handleError $ T.concat ["type does not exist", typeName]
    Just x  -> pure x
