{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Internal.Utils
  ( removeDuplicates,
    isEnum,
    withMode,
  )
where

import Data.Morpheus.Client.Internal.Types
  ( ClientConstructorDefinition (cFields),
    Mode (..),
  )
import Data.Morpheus.Types.Internal.AST (TypeDefinition (..), isNotSystemTypeName, isResolverType)
import Relude

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = fst . splitDuplicates

splitDuplicates :: Eq a => [a] -> ([a], [a])
splitDuplicates = collectElems ([], [])
  where
    collectElems :: Eq a => ([a], [a]) -> [a] -> ([a], [a])
    collectElems collected [] = collected
    collectElems (collected, errors) (x : xs)
      | x `elem` collected = collectElems (collected, errors <> [x]) xs
      | otherwise = collectElems (collected <> [x], errors) xs

isEnum :: [ClientConstructorDefinition] -> Bool
isEnum = all (null . cFields)

withMode :: Mode -> TypeDefinition k s -> Bool
withMode Global t = not (isResolverType t) && isNotSystemTypeName (typeName t)
withMode Local t = isResolverType t
withMode Both _ = True
