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
    toHashMap,
  )
where

import Control.Monad.Except (MonadError)
import qualified Data.HashMap.Lazy as HM
import Data.Mergeable
  ( Merge (..),
    NameCollision (..),
  )
import Data.Mergeable.IsMap (IsMap)
import Data.Morpheus.Ext.Empty (Empty)
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
    ( IsMap k,
      Empty
    )

toHashMap :: SafeHashMap k a -> HashMap k a
toHashMap = unpackSafeHashMap

instance (Lift a, Lift k, Eq k, Hashable k) => Lift (SafeHashMap k a) where
  lift (SafeHashMap x) = let ls = HM.toList x in [|SafeHashMap (HM.fromList ls)|]

#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (SafeHashMap x) = let ls = HM.toList x in [||SafeHashMap (HM.fromList ls)||]
#endif

instance (NameCollision e a, Monad m, Hashable k, Eq k, MonadError e m) => Merge m (SafeHashMap k a) where
  merge (SafeHashMap x) (SafeHashMap y) = SafeHashMap <$> merge x y
