{-# LANGUAGE OverloadedStrings #-}

module Data.Morpheus.Error.Fragment
  ( cycleOnFragment
  , fragmentError
  ) where

-- import Data.Morpheus.Error.Utils (errorMessage)
import           Data.Morpheus.Types.Core  (EnhancedKey (..))
import           Data.Morpheus.Types.Error (GQLError (..), GQLErrors, MetaError (..))

-- import Data.Morpheus.Types.MetaInfo (MetaInfo(..))
import qualified Data.Text                 as T

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
fragmentError :: MetaError -> GQLErrors
fragmentError (UnknownType _)    = cycleOnFragment [] -- TODO should not Apairs
fragmentError (UnknownField _)   = cycleOnFragment [] -- TODO should not Apairs
fragmentError (TypeMismatch _ _) = cycleOnFragment [] -- TODO find better solution

cycleOnFragment :: [EnhancedKey] -> GQLErrors
cycleOnFragment fragments = [GQLError {desc = text, posIndex = map location fragments}]
  where
    text =
      T.concat
        [ "Cannot spread fragment \""
        , uid $ head fragments
        , "\" within itself via "
        , T.intercalate "," (map uid fragments)
        , "."
        ]
