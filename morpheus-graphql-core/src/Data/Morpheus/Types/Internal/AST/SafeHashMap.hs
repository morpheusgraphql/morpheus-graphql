{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.SafeHashMap
  ( SafeHashMap,
    toHashMap,
    safeInsert,
  )
where

-- MORPHEUS
import Data.Foldable (Foldable (..))
import Data.Functor ((<$>), Functor (..))
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import Data.Morpheus.Error.NameCollision (NameCollision (..))
import Data.Morpheus.Internal.Utils
  ( Collection (..),
    Failure (..),
    KeyOf (..),
    Listable (..),
    Merge (..),
    Selectable (..),
    toPair,
  )
import Data.Morpheus.Types.Internal.AST.Base (GQLErrors)
import Data.Morpheus.Types.Internal.AST.OrdMap
  ( safeJoin,
    safeUnionWith,
  )
import Data.Traversable (Traversable (..))
import Language.Haskell.TH.Syntax (Lift (..))
import Prelude
  ( (.),
    Eq,
    Show,
  )

newtype SafeHashMap k a = SafeHashMap
  { toHashMap :: HashMap k a
  }
  deriving
    ( Show,
      Eq,
      Functor,
      Foldable,
      Traversable
    )

deriving newtype instance
  ( Hashable k,
    KeyOf k a
  ) =>
  Collection a (SafeHashMap k a)

deriving newtype instance
  ( Hashable k,
    KeyOf k a
  ) =>
  Selectable k a (SafeHashMap k a)

instance (Lift a, Lift k, Eq k, Hashable k) => Lift (SafeHashMap k a) where
  lift (SafeHashMap x) = let ls = HM.toList x in [|SafeHashMap (HM.fromList ls)|]

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (SafeHashMap x) = let ls = HM.toList x in [||SafeHashMap (HM.fromList ls)||]
#endif

instance (NameCollision a, Eq k, Hashable k) => Merge (SafeHashMap k a) where
  merge _ (SafeHashMap x) (SafeHashMap y) = SafeHashMap <$> safeJoin x y

instance (NameCollision a, KeyOf k a, Hashable k) => Listable a (SafeHashMap k a) where
  fromElems = fmap SafeHashMap . safeUnionWith HM.empty . fmap toPair
  elems = HM.elems . toHashMap

safeInsert ::
  ( Hashable k,
    NameCollision a,
    KeyOf k a,
    Functor m,
    Failure GQLErrors m
  ) =>
  a ->
  SafeHashMap k a ->
  m (SafeHashMap k a)
safeInsert x (SafeHashMap hm) = SafeHashMap <$> safeUnionWith hm [toPair x]
