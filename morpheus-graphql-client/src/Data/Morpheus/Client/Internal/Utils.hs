{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Internal.Utils
  ( removeDuplicates,
    isEnum,
    withMode,
    getSource,
    handleResult,
    getFile,
  )
where

import Control.Monad.Except (MonadError (catchError))
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List (isSuffixOf)
import Data.Morpheus.Client.Internal.Types
  ( ClientConstructorDefinition (cFields),
    Mode (..),
    SchemaSource (..),
  )
import Data.Morpheus.Error (gqlWarnings, renderGQLErrors)
import Data.Morpheus.Internal.Ext (GQLResult, Result (..))
import Data.Morpheus.Types.Internal.AST (TypeDefinition (..), isNotSystemTypeName, isResolverType)
import qualified Data.Text.IO as TIO
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

parseSource :: FilePath -> Q SchemaSource
parseSource p
  | ".json" `isSuffixOf` p = JSON <$> readWith L.readFile p
  | ".gql" `isSuffixOf` p || ".graphql" `isSuffixOf` p = GQL <$> readWith L.readFile p
  | otherwise = fail "unsupported file format!"

getFile :: Q FilePath -> Q Text
getFile qPath = qPath >>= readWith TIO.readFile

readWith :: (FilePath -> IO a) -> FilePath -> Q a
readWith f p = do
  qAddDependentFile p
  file <- runIO (catchError ((fmap Right . f) p) (pure . Left . show))
  case file of
    Left x -> fail x
    Right x -> pure x

getSource :: Q FilePath -> Q SchemaSource
getSource qPath = qPath >>= parseSource

handleResult :: GQLResult t -> (t -> Q a) -> Q a
handleResult x f = case x of
  Failure errors -> fail (renderGQLErrors errors)
  Success
    { result,
      warnings
    } -> gqlWarnings warnings >> f result
