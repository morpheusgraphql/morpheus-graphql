{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Fragment
        ( unknownFragment
        , unsupportedSpreadOnType
        , cycleOnFragment
        )
where

import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..), Position(..) , LineMarks)
import qualified Data.Text                     as T
import           Data.Morpheus.Types.Error      ( GQLError(..)
                                                , GQLErrors
                                                )
import           Data.Morpheus.Error.Utils      ( errorMessage )

unknownFragment :: LineMarks -> MetaInfo -> GQLErrors
unknownFragment lines meta = errorMessage lines (psoition meta) text
        where text = T.concat ["Unknown fragment \"", key meta, "\"."]

unsupportedSpreadOnType :: MetaInfo -> MetaInfo -> GQLErrors
unsupportedSpreadOnType parent spread = errorMessage $ T.concat
        [ "cant apply fragment \""
        , key spread
        , "\" with type \""
        , className spread
        , "\" on type \""
        , className parent
        , "\"."
        ]

cycleOnFragment :: [T.Text] -> GQLErrors
cycleOnFragment fragments = errorMessage $ T.concat
        [ "fragment \""
        , head fragments
        , "\" has cycle \""
        , T.intercalate "," fragments
        , "\"."
        ]
