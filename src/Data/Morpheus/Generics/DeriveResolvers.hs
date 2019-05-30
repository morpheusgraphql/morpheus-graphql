{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Morpheus.Generics.DeriveResolvers
  ( DeriveResolvers(..)
  , resolversBy
  , resolveBySelection
  , resolveBySelectionM
  ) where

import           Data.Maybe                                 (fromMaybe)
import           Data.Morpheus.Types.Internal.AST.Selection (Selection (..), SelectionRec (..))
import           Data.Morpheus.Types.Internal.Validation    (ResolveIO)
import           Data.Morpheus.Types.Internal.Value         (Value (..))
import           Data.Morpheus.Types.Resolver               (WithEffect (..))
import           Data.Text                                  (Text, pack)
import           GHC.Generics

-- type D1 = M1 D
-- type C1 = M1 C
-- type S1 = M1 S
-- M1 : Meta-information (constructor names, etc.)
-- D  :Datatype : Class for dataTypes that represent dataTypes
-- C :Constructor :
-- S - Selector: Class for dataTypes that represent records
-- Rep = D1 (...)  (C1 ...) (S1 (...) :+: D1 (...)  (C1 ...) (S1 (...)
unwrapMonadTuple :: Monad m => (Text, m a) -> m (Text, a)
unwrapMonadTuple (text, ioa) = ioa >>= \x -> pure (text, x)

type ContextRes = WithEffect Value

type QueryRes = Value

type SelectRes a = [(Text, (Text, Selection) -> ResolveIO a)] -> (Text, Selection) -> ResolveIO (Text, a)

type ResolveSel a = [(Text, Selection)] -> [(Text, (Text, Selection) -> ResolveIO a)] -> ResolveIO a

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

resolversBy :: (Generic a, DeriveResolvers (Rep a) res) => a -> [(Text, (Text, Selection) -> ResolveIO res)]
resolversBy = deriveResolvers "" . from

class DeriveResolvers f res where
  deriveResolvers :: Text -> f a -> [(Text, (Text, Selection) -> ResolveIO res)]

instance DeriveResolvers U1 res where
  deriveResolvers _ _ = []

instance (Selector s, DeriveResolvers f res) => DeriveResolvers (M1 S s f) res where
  deriveResolvers _ m@(M1 src) = deriveResolvers (pack $ selName m) src

instance DeriveResolvers f res => DeriveResolvers (M1 D c f) res where
  deriveResolvers key' (M1 src) = deriveResolvers key' src

instance DeriveResolvers f res => DeriveResolvers (M1 C c f) res where
  deriveResolvers key' (M1 src) = deriveResolvers key' src

instance (DeriveResolvers f res, DeriveResolvers g res) => DeriveResolvers (f :*: g) res where
  deriveResolvers meta (a :*: b) = deriveResolvers meta a ++ deriveResolvers meta b
