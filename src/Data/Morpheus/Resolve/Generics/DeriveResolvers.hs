{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Morpheus.Resolve.Generics.DeriveResolvers
  ( ObjectFieldResolvers(..)
  , UnionResolvers(..)
  , resolversBy
  , resolveBySelection
  , resolveBySelectionM
  , lookupSelectionByType
  ) where

import           Data.Maybe                                 (fromMaybe)
import           Data.Morpheus.Types.Internal.AST.Selection (Selection (..), SelectionRec (..), SelectionSet)
import           Data.Morpheus.Types.Internal.Validation    (ResolveIO)
import           Data.Morpheus.Types.Internal.Value         (Value (..))
import           Data.Morpheus.Types.Resolver               (WithEffect (..))
import           Data.Text                                  (Text, pack)
import           GHC.Generics

type ContextRes = WithEffect Value

type QueryRes = Value

type SelectRes a = [(Text, (Text, Selection) -> ResolveIO a)] -> (Text, Selection) -> ResolveIO (Text, a)

type ResolveSel a = [(Text, Selection)] -> [(Text, (Text, Selection) -> ResolveIO a)] -> ResolveIO a

--
-- OBJECT
--
class ObjectFieldResolvers f res where
  objectFieldResolvers :: Text -> f a -> [(Text, (Text, Selection) -> ResolveIO res)]

instance ObjectFieldResolvers U1 res where
  objectFieldResolvers _ _ = []

instance (Selector s, ObjectFieldResolvers f res) => ObjectFieldResolvers (M1 S s f) res where
  objectFieldResolvers _ m@(M1 src) = objectFieldResolvers (pack $ selName m) src

instance ObjectFieldResolvers f res => ObjectFieldResolvers (M1 D c f) res where
  objectFieldResolvers key' (M1 src) = objectFieldResolvers key' src

instance ObjectFieldResolvers f res => ObjectFieldResolvers (M1 C c f) res where
  objectFieldResolvers key' (M1 src) = objectFieldResolvers key' src

instance (ObjectFieldResolvers f res, ObjectFieldResolvers g res) => ObjectFieldResolvers (f :*: g) res where
  objectFieldResolvers meta (a :*: b) = objectFieldResolvers meta a ++ objectFieldResolvers meta b

unwrapMonadTuple :: Monad m => (Text, m a) -> m (Text, a)
unwrapMonadTuple (text, ioa) = ioa >>= \x -> pure (text, x)

selectResolver :: a -> SelectRes a
selectResolver defaultValue resolvers' (key', selection') =
  case selectionRec selection' of
    SelectionAlias name' aliasSelection' ->
      unwrapMonadTuple (key', lookupResolver name' (selection' {selectionRec = aliasSelection'}))
    _ -> unwrapMonadTuple (key', lookupResolver key' selection')
  where
    lookupResolver resolverKey' sel =
      (fromMaybe (const $ return $defaultValue) $ lookup resolverKey' resolvers') (key', sel)

resolveBySelection :: ResolveSel QueryRes
resolveBySelection selection resolvers = Object <$> mapM (selectResolver Null resolvers) selection

resolveBySelectionM :: ResolveSel ContextRes
resolveBySelectionM selection resolvers = do
  value <- mapM (selectResolver (return Null) resolvers) selection
  let value' = fmap (\(x, v) -> (x, resultValue v)) value
  let effects = concatMap (resultEffects . snd) value
  return $ WithEffect effects (Object value')

resolversBy :: (Generic a, ObjectFieldResolvers (Rep a) res) => a -> [(Text, (Text, Selection) -> ResolveIO res)]
resolversBy = objectFieldResolvers "" . from

--
-- UNION
--
-- SPEC: if there is no any fragment that supports current object Type GQL returns {}
lookupSelectionByType :: Text -> [(Text, SelectionSet)] -> SelectionSet
lookupSelectionByType type' sel = fromMaybe [] $ lookup type' sel

class UnionResolvers f res where
  unionResolvers :: f a -> (Text, (Text, Selection) -> ResolveIO res)

instance UnionResolvers f res => UnionResolvers (M1 S s f) res where
  unionResolvers (M1 x) = unionResolvers x

instance UnionResolvers f res => UnionResolvers (M1 D c f) res where
  unionResolvers (M1 x) = unionResolvers x

instance UnionResolvers f res => UnionResolvers (M1 C c f) res where
  unionResolvers (M1 x) = unionResolvers x

instance (UnionResolvers a res, UnionResolvers b res) => UnionResolvers (a :+: b) res where
  unionResolvers (L1 x) = unionResolvers x
  unionResolvers (R1 x) = unionResolvers x
