{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Internal.Utils
  ( removeDuplicates,
    isEnum,
    isLocalType,
    isGlobalType,
    withMode,
  )
where

import Data.Morpheus.Client.Internal.Types
  ( ClientConstructorDefinition (cFields),
    ClientTypeDefinition (..),
    Mode (..),
  )
import Data.Morpheus.CodeGen.Internal.AST (TypeKind (..))
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

isLocalType :: TypeKind -> Bool
isLocalType KindObject {} = True
isLocalType KindUnion {} = True
isLocalType KindInterface {} = True
isLocalType _ = False

isGlobalType :: TypeKind -> Bool
isGlobalType KindScalar {} = True
isGlobalType KindEnum {} = True
isGlobalType KindInputObject {} = True
isGlobalType KindInputUnion {} = True
isGlobalType _ = False

withMode :: Mode -> [ClientTypeDefinition] -> [ClientTypeDefinition]
withMode Global = filter (\ClientTypeDefinition {clientKind} -> isGlobalType clientKind)
withMode Local = filter (\ClientTypeDefinition {clientKind} -> isLocalType clientKind)
withMode Both = id
