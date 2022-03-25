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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Utils.GTraversable where

import qualified Data.Map as M
import Data.Morpheus.Kind
import Data.Morpheus.NamedResolvers (NamedResolverT)
import Data.Morpheus.Server.Deriving.Utils.Kinded
import Data.Morpheus.Server.Types.GQLType (GQLType (KIND, __type), TypeData (gqlFingerprint))
import Data.Morpheus.Server.Types.SchemaT (TypeFingerprint)
import Data.Morpheus.Types.Internal.AST
import GHC.Generics
import Relude hiding (Undefined)

traverseTypes ::
  (GFmap (ScanConstraint c) (KIND a) a, c (KIND a) a, GQLType a) =>
  Mappable c v KindedProxy ->
  Proxy a ->
  Map TypeFingerprint v
traverseTypes f = runMappable (scanner f mempty) . withDerivable

class
  (GFmap (ScanConstraint c) (KIND a) a, c (KIND a) a) =>
  ScanConstraint
    (c :: DerivingKind -> Type -> Constraint)
    (k :: DerivingKind)
    (a :: Type)

instance (GFmap (ScanConstraint c) (KIND a) a, c (KIND a) a) => ScanConstraint c k a

scanner ::
  Mappable c v KindedProxy ->
  Map TypeFingerprint v ->
  Mappable (ScanConstraint c) (Map TypeFingerprint v) KindedProxy
scanner c@(Mappable f) lib =
  Mappable
    ( \proxy -> do
        let typeInfo = __type proxy OUT
        let fingerprint = gqlFingerprint typeInfo
        if M.member fingerprint lib
          then lib
          else do
            let newLib = M.insert fingerprint (f proxy) lib
            gfmap (scanner c newLib) proxy
    )

withDerivable :: proxy a -> KindedProxy (KIND a) a
withDerivable _ = KindedProxy

newtype Mappable (c :: DerivingKind -> Type -> Constraint) (v :: Type) (f :: DerivingKind -> Type -> Type) = Mappable
  { runMappable :: forall a. (GQLType a, c (KIND a) a) => KindedProxy (KIND a) a -> v
  }

-- Map
class GFmap (c :: DerivingKind -> Type -> Constraint) (t :: DerivingKind) a where
  gfmap :: (Monoid v, Semigroup v) => Mappable c v KindedProxy -> kinded t a -> v

instance (GQLType a, c (KIND a) a) => GFmap c SCALAR a where
  gfmap (Mappable f) _ = f (KindedProxy :: KindedProxy (KIND a) a)

instance (GQLType a, c (KIND a) a, GFunctor c (Rep a)) => GFmap c TYPE a where
  gfmap f@(Mappable fx) _ = fx (KindedProxy :: KindedProxy (KIND a) a) <> genericMap f (Proxy @(Rep a))

instance GFmap c (KIND a) a => GFmap c WRAPPER (f a) where
  gfmap f _ = gfmap f (KindedProxy :: KindedProxy (KIND a) a)

instance GFmap c (KIND a) a => GFmap c CUSTOM (input -> a) where
  gfmap f _ = gfmap f (KindedProxy :: KindedProxy (KIND a) a)

instance GFmap c (KIND a) a => GFmap c CUSTOM (NamedResolverT m a) where
  gfmap f _ = gfmap f (KindedProxy :: KindedProxy (KIND a) a)

--
--
-- GFunctor
--
--
class GFunctor (c :: DerivingKind -> Type -> Constraint) a where
  genericMap :: (Monoid v, Semigroup v) => Mappable c v p -> proxy a -> v

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
