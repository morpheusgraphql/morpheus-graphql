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

withMode :: Mode -> TypeDefinition k s -> Bool
withMode Global t = isResolverType t && isNotSystemTypeName (typeName t)
withMode Local t = not (isResolverType t)
withMode Both _ = True
