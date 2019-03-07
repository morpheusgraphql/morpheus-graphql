{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Fragment where

import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..) )
import qualified Data.Text                     as T
import           Data.Morpheus.Types.Error      ( GQLError(..))
import           Data.Morpheus.Error.Utils      (errorMessage)


unknownFragment :: MetaInfo -> [GQLError]
unknownFragment meta = errorMessage $ T.concat ["Unknown fragment \"", key meta, "\"."]
    
unsupportedSpreadOnType :: MetaInfo -> MetaInfo -> [GQLError]
unsupportedSpreadOnType parent spread = errorMessage $ T.concat [ "cant apply fragment \""
        , key spread
        , "\" with type \""
        , className spread
        , "\" on type \""
        , className parent
        , "\"."]