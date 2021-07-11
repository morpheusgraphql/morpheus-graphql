{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Mergeable.SafeHashMap
  ( SafeHashMap,
    unsafeFromList,
    insert,
    toHashMap,
  )
where

import qualified Data.HashMap.Lazy as HM
import Data.Mergeable
  ( Merge (..),
    NameCollision (..),
  )
import Data.Morpheus.Ext.Empty (Empty)
import Data.Morpheus.Internal.Utils
  ( Collection (..),
    Failure (..),
    FromElems (..),
    IsMap (..),
    KeyOf (..),
  )
import Data.Morpheus.Types.Internal.AST.Error (ValidationErrors)
import Language.Haskell.TH.Syntax (Lift (..))
import Relude

newtype SafeHashMap k a = SafeHashMap
  { unpackSafeHashMap :: HashMap k a
  }
  deriving
    ( Show,
      Eq,
      Functor,
      Foldable,
      Traversable
    )
  deriving newtype
    ( Collection a,
      IsMap k,
      Empty
    )

instance (Lift a, Lift k, Eq k, Hashable k) => Lift (SafeHashMap k a) where
  lift (SafeHashMap x) = let ls = HM.toList x in [|SafeHashMap (HM.fromList ls)|]

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (SafeHashMap x) = let ls = HM.toList x in [||SafeHashMap (HM.fromList ls)||]
#endif

instance (NameCollision a, Monad m, Hashable k, Eq k, Failure ValidationErrors m) => Merge m (SafeHashMap k a) where
  merge (SafeHashMap x) (SafeHashMap y) = SafeHashMap <$> merge x y

instance (NameCollision a, Failure ValidationErrors m, Monad m, KeyOf k a, Hashable k) => FromElems m a (SafeHashMap k a) where
  fromElems = fmap SafeHashMap . fromElems

toHashMap :: SafeHashMap k a -> HashMap k a
toHashMap = unpackSafeHashMap

unsafeFromList :: (Eq k, Hashable k) => [(k, a)] -> SafeHashMap k a
unsafeFromList = SafeHashMap . HM.fromList

insert ::
  ( NameCollision a,
    KeyOf k a,
    Monad m,
    Failure ValidationErrors m
  ) =>
  a ->
  SafeHashMap k a ->
  m (SafeHashMap k a)
insert x = merge (singleton x)
