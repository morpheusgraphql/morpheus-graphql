{-# LANGUAGE NamedFieldPuns #-}


module Data.Morpheus.Validation.Internal.Utils
  ( differKeys
  , checkNameCollision
  , checkForUnknownKeys
  , VALIDATION_MODE(..)
  )
where

import           Data.List                      ( (\\) )
import           Data.Morpheus.Types.Internal.Base
                                                ( Ref(..)
                                                , Key
                                                , enhanceKeyWithNull
                                                )
import qualified Data.Set                      as S
import           Data.Text                      ( Text )

data VALIDATION_MODE
  = WITHOUT_VARIABLES
  | FULL_VALIDATION
  deriving (Eq, Show)

differKeys :: [Ref] -> [Key] -> [Ref]
differKeys enhanced keys = enhanced \\ map enhanceKeyWithNull keys

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = S.toList . S.fromList

elementOfKeys :: [Text] -> Ref -> Bool
elementOfKeys keys Ref { refName } = refName `elem` keys

checkNameCollision :: [Ref] -> ([Ref] -> error) -> Either error [Ref]
checkNameCollision enhancedKeys errorGenerator =
  case enhancedKeys \\ removeDuplicates enhancedKeys of
    []         -> pure enhancedKeys
    duplicates -> Left $ errorGenerator duplicates

checkForUnknownKeys :: [Ref] -> [Text] -> ([Ref] -> error) -> Either error [Ref]
checkForUnknownKeys enhancedKeys' keys' errorGenerator' =
  case filter (not . elementOfKeys keys') enhancedKeys' of
    []           -> pure enhancedKeys'
    unknownKeys' -> Left $ errorGenerator' unknownKeys'
