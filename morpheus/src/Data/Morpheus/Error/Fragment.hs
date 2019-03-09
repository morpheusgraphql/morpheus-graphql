{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Fragment
        ( unknownFragment
        , unsupportedSpreadOnType
        , cycleOnFragment
        )
where

import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..) )
import qualified Data.Text                     as T
import           Data.Morpheus.Types.Error      ( GQLError(..) )
import           Data.Morpheus.Error.Utils      ( errorMessage
                                                , Errors
                                                )


unknownFragment :: MetaInfo -> Errors
unknownFragment meta =
        errorMessage $ T.concat ["Unknown fragment \"", key meta, "\"."]

unsupportedSpreadOnType :: MetaInfo -> MetaInfo -> Errors
unsupportedSpreadOnType parent spread = errorMessage $ T.concat
        [ "cant apply fragment \""
        , key spread
        , "\" with type \""
        , className spread
        , "\" on type \""
        , className parent
        , "\"."
        ]

cycleOnFragment :: [T.Text] -> Errors
cycleOnFragment fragments = errorMessage $ T.concat
        [ "fragment \""
        , head fragments
        , "\" has cycle \""
        , T.intercalate "," fragments
        , "\"."
        ]
