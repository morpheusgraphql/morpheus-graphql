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

module Data.Morpheus.Ext.SafeHashMap
  ( SafeHashMap,
    unsafeFromList,
    insert,
  )
where

-- MORPHEUS
import Control.Monad (Monad)
import Data.Foldable (Foldable (..))
import Data.Functor ((<$>), Functor (..))
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import Data.Morpheus.Error.NameCollision (NameCollision (..))
import Data.Morpheus.Ext.Elems (Elems)
import Data.Morpheus.Ext.SemigroupM
  ( (<:>),
    SemigroupM (..),
  )
import Data.Morpheus.Internal.Utils
  ( Collection (..),
    Failure (..),
    FromElems (..),
    KeyOf (..),
    Selectable (..),
  )
import Data.Morpheus.Types.Internal.AST.Base (ValidationErrors)
import Data.Traversable (Traversable (..))
import Language.Haskell.TH.Syntax (Lift (..))
import Prelude
  ( (.),
    Eq,
    Show,
  )

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
      Selectable k a,
      Elems a
    )

instance (Lift a, Lift k, Eq k, Hashable k) => Lift (SafeHashMap k a) where
  lift (SafeHashMap x) = let ls = HM.toList x in [|SafeHashMap (HM.fromList ls)|]

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (SafeHashMap x) = let ls = HM.toList x in [||SafeHashMap (HM.fromList ls)||]
#endif

instance (NameCollision a, Monad m, KeyOf k a, Failure ValidationErrors m) => SemigroupM m (SafeHashMap k a) where
  mergeM ref (SafeHashMap x) (SafeHashMap y) = SafeHashMap <$> mergeM ref x y

instance (NameCollision a, Failure ValidationErrors m, Monad m, KeyOf k a, Hashable k) => FromElems m a (SafeHashMap k a) where
  fromElems = fmap SafeHashMap . fromElems

unsafeFromList :: (Eq k, KeyOf k a) => [(k, a)] -> SafeHashMap k a
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
insert x = (<:> singleton x)
