{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Utils.GTraversble where

import qualified Data.Map as M
import Data.Morpheus.Kind
import Data.Morpheus.Server.Deriving.Utils.Kinded
import Data.Morpheus.Server.Types.GQLType (GQLType (KIND, __type), TypeData (gqlFingerprint))
import Data.Morpheus.Server.Types.SchemaT (TypeFingerprint)
import Data.Morpheus.Types.Internal.AST
import GHC.Generics
import Relude hiding (Undefined)

-- Traverse
traverseTypesM ::
  ( GFmap (ScanConstraint c) (KIND a) a,
    c (KIND a) a,
    GQLType a,
    Monad m
  ) =>
  Mappable c (m v) KindedProxy ->
  Proxy a ->
  m (Map TypeFingerprint v)
traverseTypesM f = unpackM . runMappable (scanner f mempty) . withDerivable

unpackM :: Monad m => Map TypeFingerprint (m v) -> m (Map TypeFingerprint v)
unpackM = fxx . M.toList

fxx :: Monad m => [(TypeFingerprint, m a)] -> m (Map TypeFingerprint a)
fxx xs = M.fromList <$> traverse fx xs

fx :: Functor f => (t1, f t2) -> f (t1, t2)
fx (fp, x) = (fp,) <$> x

traverseTypes ::
  (GFmap (ScanConstraint c) (KIND a) a, c (KIND a) a, GQLType a) =>
  Mappable c v KindedProxy ->
  Proxy a ->
  Map TypeFingerprint v
traverseTypes f = runMappable (scanner f mempty) . withDerivable

class
  (GFmap (ScanConstraint c) (KIND a) a, c (KIND a) a) =>
  ScanConstraint
    (c :: DerivingKind -> * -> Constraint)
    (k :: DerivingKind)
    (a :: *)

instance (GFmap (ScanConstraint c) (KIND a) a, c (KIND a) a) => ScanConstraint c k a

scanner ::
  Mappable c v KindedProxy ->
  Map TypeFingerprint v ->
  Mappable (ScanConstraint c) (Map TypeFingerprint v) KindedProxy
scanner c@(Mappable f) lib =
  Mappable
    ( \proxy -> do
        let fingerprint = gqlFingerprint (__type proxy OUT)
        if M.member fingerprint lib
          then lib
          else gfmap (scanner c (M.insert fingerprint (f proxy) lib)) proxy
    )

withDerivable :: proxy a -> KindedProxy (KIND a) a
withDerivable _ = KindedProxy

newtype Mappable (c :: DerivingKind -> * -> Constraint) (v :: *) (f :: DerivingKind -> * -> *) = Mappable
  { runMappable :: forall a. (GQLType a, c (KIND a) a) => KindedProxy (KIND a) a -> v
  }

-- Map
class GFmap (c :: DerivingKind -> * -> Constraint) (t :: DerivingKind) a where
  gfmap :: Monoid v => Mappable c v KindedProxy -> kinded t a -> v

instance GFunctor c (Rep a) => GFmap c TYPE a where
  gfmap f _ = genericMap f (Proxy @(Rep a))

instance GFmap c (KIND a) a => GFmap c WRAPPER (f a) where
  gfmap f _ = gfmap f (KindedProxy :: KindedProxy (KIND a) a)

instance (GQLType a, c (KIND a) a) => GFmap c SCALAR a where
  gfmap (Mappable f) _ = f (KindedProxy :: KindedProxy (KIND a) a)

instance (GQLType a, c (KIND a) a) => GFmap c CUSTOM a where
  gfmap (Mappable f) _ = f (KindedProxy :: KindedProxy (KIND a) a)

--
--
-- GFunctor
--
--
class GFunctor (c :: DerivingKind -> * -> Constraint) a where
  genericMap :: Monoid v => Mappable c v p -> proxy a -> v

instance (Datatype d, GFunctor c a) => GFunctor c (M1 D d a) where
  genericMap fun _ = genericMap fun (Proxy @a)

instance (GFunctor con a) => GFunctor con (M1 C c a) where
  genericMap f _ = genericMap f (Proxy @a)

instance (GFunctor c a, GFunctor c b) => GFunctor c (a :+: b) where
  genericMap fun _ = genericMap fun (Proxy @a) <> genericMap fun (Proxy @b)

instance (GFunctor c a, GFunctor c b) => GFunctor c (a :*: b) where
  genericMap fun _ = genericMap fun (Proxy @a) <> genericMap fun (Proxy @b)

instance (GQLType a, c (KIND a) a) => GFunctor c (M1 S s (K1 x a)) where
  genericMap (Mappable f) _ = f (KindedProxy :: KindedProxy (KIND a) a)

instance GFunctor c U1 where
  genericMap _ _ = mempty
