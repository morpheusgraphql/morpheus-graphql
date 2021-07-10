{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Ext.SemigroupM
  ( SemigroupM (..),
    (<:>),
    join,
  )
where

import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.Error.NameCollision (NameCollision)
import Data.Morpheus.Ext.KeyOf (KeyOf (..))
import Data.Morpheus.Ext.Map
  ( fromListT,
    runResolutionT,
  )
import Data.Morpheus.Internal.Utils
  ( Failure,
    failOnDuplicates,
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( Ref,
  )
import Data.Morpheus.Types.Internal.AST.Error
  ( ValidationErrors,
  )
import Data.Morpheus.Types.Internal.AST.Name
  ( FieldName,
  )
import Relude hiding (empty, join)

class SemigroupM (m :: * -> *) a where
  mergeM :: [Ref FieldName] -> a -> a -> m a

instance
  ( NameCollision a,
    Monad m,
    KeyOf k a,
    Failure ValidationErrors m
  ) =>
  SemigroupM m (HashMap k a)
  where
  mergeM _ x y = runResolutionT (fromListT $ HM.toList x <> HM.toList y) HM.fromList failOnDuplicates

join ::
  ( Monad m,
    Failure ValidationErrors m,
    SemigroupM m a
  ) =>
  NonEmpty a ->
  m a
join (value :| []) = pure value
join (value :| (x : xs)) = do
  a <- value <:> x
  join (a :| xs)

(<:>) :: SemigroupM m a => a -> a -> m a
(<:>) = mergeM []
