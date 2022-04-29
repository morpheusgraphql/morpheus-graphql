{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Internal.Utils
  ( removeDuplicates,
    isEnum,
    withMode,
    getSource,
  )
where

import qualified Data.ByteString.Lazy.Char8 as L
import Data.List (isSuffixOf)
import Data.Morpheus.Client.Internal.Types
  ( ClientConstructorDefinition (cFields),
    Mode (..),
    Source (..),
  )
import Data.Morpheus.Types.Internal.AST (TypeDefinition (..), isNotSystemTypeName, isResolverType)
import Language.Haskell.TH (Q, runIO)
import Language.Haskell.TH.Syntax (qAddDependentFile)
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
withMode Legacy _ = True

parseSource :: FilePath -> IO Source
parseSource p
  | ".json" `isSuffixOf` p = JSON <$> L.readFile p
  | ".gql" `isSuffixOf` p || ".graphql" `isSuffixOf` p = GQL <$> L.readFile p
  | otherwise = fail "unsupported file format!"

getSource :: Q FilePath -> Q Source
getSource qPath = do
  p <- qPath
  qAddDependentFile p
  runIO (parseSource p)
