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
                                                ( Reference(..)
                                                , Key
                                                , enhanceKeyWithNull
                                                )
import qualified Data.Set                      as S
import           Data.Text                      ( Text )

data VALIDATION_MODE
  = WITHOUT_VARIABLES
  | FULL_VALIDATION
  deriving (Eq, Show)

differKeys :: [Reference] -> [Key] -> [Reference]
differKeys enhanced keys = enhanced \\ map enhanceKeyWithNull keys

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = S.toList . S.fromList

elementOfKeys :: [Text] -> Reference -> Bool
elementOfKeys keys Reference { refName } = refName `elem` keys

checkNameCollision
  :: [Reference] -> ([Reference] -> error) -> Either error [Reference]
checkNameCollision enhancedKeys errorGenerator =
  case enhancedKeys \\ removeDuplicates enhancedKeys of
    []         -> pure enhancedKeys
    duplicates -> Left $ errorGenerator duplicates

checkForUnknownKeys
  :: [Reference] -> [Text] -> ([Reference] -> error) -> Either error [Reference]
checkForUnknownKeys enhancedKeys' keys' errorGenerator' =
  case filter (not . elementOfKeys keys') enhancedKeys' of
    []           -> pure enhancedKeys'
    unknownKeys' -> Left $ errorGenerator' unknownKeys'
