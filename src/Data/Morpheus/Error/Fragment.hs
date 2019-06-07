{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Fragment
  ( cannotSpreadWithinItself
  ) where

-- import Data.Morpheus.Error.Utils (errorMessage)
import           Data.Morpheus.Types.Internal.Base       (EnhancedKey (..))
import           Data.Morpheus.Types.Internal.Validation (GQLError (..), GQLErrors)

-- import Data.Morpheus.Types.Internal.Base (MetaInfo(..))
import qualified Data.Text                               as T

{-
  FRAGMENT:
    type Experience {
        experience ( lang: LANGUAGE ) : String ,
        date: String
    }
    fragment type mismatch -> "Fragment \"H\" cannot be spread here as objects of type \"Hobby\" can never be of type \"Experience\"."
    fragment H on T1 { ...A} , fragment A on T { ...H } -> "Cannot spread fragment \"H\" within itself via A."
    fragment H on D {...}  ->  "Unknown type \"D\"."
    {...H} -> "Unknown fragment \"H\"."
-}
cannotSpreadWithinItself :: [EnhancedKey] -> GQLErrors
cannotSpreadWithinItself fragments = [GQLError {desc = text, positions = map location fragments}]
  where
    text =
      T.concat
        [ "Cannot spread fragment \""
        , uid $ head fragments
        , "\" within itself via "
        , T.intercalate "," (map uid fragments)
        , "."
        ]
