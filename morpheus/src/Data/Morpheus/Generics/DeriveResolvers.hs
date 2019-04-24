{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

module Data.Morpheus.Generics.DeriveResolvers
  ( DeriveResolvers(..)
  , resolveBySelection
  ) where

import           Data.Maybe                          (fromMaybe)
import           Data.Morpheus.Types.Error           (ResolveIO)
import           Data.Morpheus.Types.JSType          (JSType (..))
import qualified Data.Morpheus.Types.MetaInfo        as Meta (MetaInfo (..))
import           Data.Morpheus.Types.Query.Selection (Selection)
import qualified Data.Text                           as T
import           GHC.Generics

-- type D1 = M1 D
-- type C1 = M1 C
-- type S1 = M1 S
-- M1 : Meta-information (constructor names, etc.)
-- D  :Datatype : Class for dataTypes that represent dataTypes
-- C :Constructor :
-- S - Selector: Class for dataTypes that represent records
-- Rep = D1 (...)  (C1 ...) (S1 (...) :+: D1 (...)  (C1 ...) (S1 (...)
unwrapMonadTuple :: Monad m => (T.Text, m a) -> m (T.Text, a)
unwrapMonadTuple (text, ioa) = ioa >>= \x -> pure (text, x)

selectResolver ::
     [(T.Text, (T.Text, Selection) -> ResolveIO JSType)] -> (T.Text, Selection) -> ResolveIO (T.Text, JSType)
selectResolver x (key, gql) = unwrapMonadTuple (key, (fromMaybe (\_ -> pure JSNull) $ lookup key x) (key, gql))

resolveBySelection :: [(T.Text, Selection)] -> [(T.Text, (T.Text, Selection) -> ResolveIO JSType)] -> ResolveIO JSType
resolveBySelection selection resolvers = JSObject <$> mapM (selectResolver resolvers) selection

class DeriveResolvers f where
  deriveResolvers :: Meta.MetaInfo -> f a -> [(T.Text, (T.Text, Selection) -> ResolveIO JSType)]

instance DeriveResolvers U1 where
  deriveResolvers _ _ = []

instance (Selector s, DeriveResolvers f) => DeriveResolvers (M1 S s f) where
  deriveResolvers meta m@(M1 src) = deriveResolvers (meta {Meta.key = T.pack $ selName m}) src

instance (Datatype c, DeriveResolvers f) => DeriveResolvers (M1 D c f) where
  deriveResolvers meta m@(M1 src) = deriveResolvers (meta {Meta.typeName = T.pack $ datatypeName m}) src

instance (Constructor c, DeriveResolvers f) => DeriveResolvers (M1 C c f) where
  deriveResolvers meta (M1 src) = deriveResolvers meta src

instance (DeriveResolvers f, DeriveResolvers g) => DeriveResolvers (f :*: g) where
  deriveResolvers meta (a :*: b) = deriveResolvers meta a ++ deriveResolvers meta b
