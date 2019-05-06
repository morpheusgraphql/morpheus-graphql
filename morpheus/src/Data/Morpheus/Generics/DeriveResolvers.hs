{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Generics.DeriveResolvers
  ( DeriveResolvers(..)
  , resolveBySelection
  ) where

import           Data.Maybe                          (fromMaybe)
import           Data.Morpheus.Types.Error           (ResolveIO)
import           Data.Morpheus.Types.JSType          (JSType (..))
import           Data.Morpheus.Types.Query.Selection (Selection)
import           Data.Text                           (Text, pack)
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

selectResolver :: [(Text, (Text, Selection) -> ResolveIO JSType)] -> (Text, Selection) -> ResolveIO (Text, JSType)
selectResolver x (key, gql) = unwrapMonadTuple (key, (fromMaybe (\_ -> pure JSNull) $ lookup key x) (key, gql))

resolveBySelection :: [(Text, Selection)] -> [(Text, (Text, Selection) -> ResolveIO JSType)] -> ResolveIO JSType
resolveBySelection selection resolvers = JSObject <$> mapM (selectResolver resolvers) selection

class DeriveResolvers f where
  deriveResolvers :: Text -> f a -> [(Text, (Text, Selection) -> ResolveIO JSType)]

instance DeriveResolvers U1 where
  deriveResolvers _ _ = []

instance (Selector s, DeriveResolvers f) => DeriveResolvers (M1 S s f) where
  deriveResolvers _ m@(M1 src) = deriveResolvers (pack $ selName m) src

instance DeriveResolvers f => DeriveResolvers (M1 D c f) where
  deriveResolvers key' (M1 src) = deriveResolvers key' src

instance DeriveResolvers f => DeriveResolvers (M1 C c f) where
  deriveResolvers key' (M1 src) = deriveResolvers key' src

--instance (Datatype c, DeriveResolvers f) => DeriveResolvers (M1 D c f) where
--  deriveResolvers meta m@(M1 src) = deriveResolvers (meta {Meta.typeName = T.pack $ datatypeName m}) src
--instance (Constructor c, DeriveResolvers f) => DeriveResolvers (M1 C c f) where
--  deriveResolvers meta (M1 src) = deriveResolvers meta src
instance (DeriveResolvers f, DeriveResolvers g) => DeriveResolvers (f :*: g) where
  deriveResolvers meta (a :*: b) = deriveResolvers meta a ++ deriveResolvers meta b
