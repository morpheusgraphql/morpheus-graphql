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
unknownFragment lines meta = errorMessage lines (position meta) text
        where text = T.concat ["Unknown fragment \"", key meta, "\"."]

unsupportedSpreadOnType :: LineMarks -> MetaInfo -> MetaInfo -> GQLErrors
unsupportedSpreadOnType lines parent spread = errorMessage lines (position parent) text
        where 
            text = T.concat
                        [ "cant apply fragment \""
                        , key spread
                        , "\" with type \""
                        , typeName spread
                        , "\" on type \""
                        , typeName parent
                        , "\"."
                        ]

cycleOnFragment :: [T.Text] -> GQLErrors
cycleOnFragment fragments = errorMessage [] (Position 0) $ T.concat
        [ "fragment \""
        , head fragments
        , "\" has cycle \""
        , T.intercalate "," fragments
        , "\"."
        ]
