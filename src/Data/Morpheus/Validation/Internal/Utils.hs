{-# LANGUAGE NamedFieldPuns #-}


module Data.Morpheus.Validation.Internal.Utils
  ( differKeys
  , checkNameCollision
  , checkForUnknownKeys
  , VALIDATION_MODE(..)
  )
where

import           Data.List                      ( (\\) )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Ref(..)
                                                , Key
                                                , anonymousRef
                                                )
import qualified Data.Set                      as S
import           Data.Text                      ( Text )
import           Data.Morpheus.Types.Internal.Resolving.Core
                                                ( Failure(..) )

data VALIDATION_MODE
  = WITHOUT_VARIABLES
  | FULL_VALIDATION
  deriving (Eq, Show)

differKeys :: [Ref] -> [Key] -> [Ref]
differKeys enhanced keys = enhanced \\ map anonymousRef keys

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = S.toList . S.fromList

elementOfKeys :: [Text] -> Ref -> Bool
elementOfKeys keys Ref { refName } = refName `elem` keys

checkNameCollision :: Failure e m => [Ref] -> ([Ref] -> e) -> m [Ref]
checkNameCollision enhancedKeys errorGenerator =
  case enhancedKeys \\ removeDuplicates enhancedKeys of
    []         -> pure enhancedKeys
    duplicates -> failure $ errorGenerator duplicates

checkForUnknownKeys :: Failure e m => [Ref] -> [Text] -> ([Ref] -> e) -> m [Ref]
checkForUnknownKeys enhancedKeys' keys' errorGenerator' =
  case filter (not . elementOfKeys keys') enhancedKeys' of
    []           -> pure enhancedKeys'
    unknownKeys' -> failure $ errorGenerator' unknownKeys'
