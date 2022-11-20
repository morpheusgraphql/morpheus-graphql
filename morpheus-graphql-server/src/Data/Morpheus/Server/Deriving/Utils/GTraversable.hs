{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Utils.GTraversable
  ( Mappable (..),
    Scanner (..),
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import Data.Morpheus.Server.Deriving.Utils.Kinded
import Data.Morpheus.Server.NamedResolvers (NamedResolverT)
import Data.Morpheus.Server.Types.GQLType (GQLType (..), kindedProxy)
import Data.Morpheus.Server.Types.Internal
  ( TypeData (gqlFingerprint),
  )
import Data.Morpheus.Server.Types.Kind
import Data.Morpheus.Server.Types.SchemaT (TypeFingerprint)
import GHC.Generics
import Relude hiding (Undefined)

traverseTypes :: (Scanner c a, c a, GQLType a) => Mappable c v -> Proxy a -> Map TypeFingerprint v
traverseTypes f = mappableFun (scanner f mempty) . withDerivable

class (GFmap (Scanner c) (KIND a) a, c a) => Scanner (c :: Type -> Constraint) (a :: Type) where
  scan :: (Hashable k, Eq k, c a, GQLType a) => (b -> k) -> Mappable c [b] -> Proxy a -> HashMap k b

instance (GFmap (Scanner c) (KIND a) a, c a) => Scanner c a where
  scan keyF f xs =
    HM.fromList $
      map (\x -> (keyF x, x)) $
        join $
          toList $
            traverseTypes f xs

scanner ::
  Mappable c v ->
  Map TypeFingerprint v ->
  Mappable (Scanner c) (Map TypeFingerprint v)
scanner c@Mappable {..} lib =
  Mappable
    ( \proxy -> do
        let fingerprint = gqlFingerprint (__type (outputType proxy))
        if M.member fingerprint lib
          then lib
          else do
            let newLib = M.insert fingerprint (mappableFun proxy) lib
            gfmap (scanner c newLib) (kindedProxy proxy)
    )

withDerivable :: proxy a -> KindedProxy (KIND a) a
withDerivable _ = KindedProxy

newtype Mappable (c :: Type -> Constraint) (v :: Type) = Mappable
  { mappableFun :: forall f a. (GQLType a, c a) => f a -> v
  }

-- Map
class GFmap (c :: Type -> Constraint) (t :: DerivingKind) a where
  gfmap :: (Monoid v, Semigroup v) => Mappable c v -> kinded t a -> v

instance (GQLType a, c a) => GFmap c SCALAR a where
  gfmap Mappable {..} _ = mappableFun (KindedProxy :: KindedProxy (KIND a) a)

instance (GQLType a, c a, GFunctor c (Rep a)) => GFmap c TYPE a where
  gfmap f@Mappable {..} _ = mappableFun (KindedProxy :: KindedProxy (KIND a) a) <> genericMap f (Proxy @(Rep a))

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
class GFunctor (c :: Type -> Constraint) a where
  genericMap :: (Monoid v, Semigroup v) => Mappable c v -> proxy a -> v

instance (Datatype d, GFunctor c a) => GFunctor c (M1 D d a) where
  genericMap fun _ = genericMap fun (Proxy @a)

instance (GFunctor con a) => GFunctor con (M1 C c a) where
  genericMap f _ = genericMap f (Proxy @a)

instance (GFunctor c a, GFunctor c b) => GFunctor c (a :+: b) where
  genericMap fun _ = genericMap fun (Proxy @a) <> genericMap fun (Proxy @b)

instance (GFunctor c a, GFunctor c b) => GFunctor c (a :*: b) where
  genericMap fun _ = genericMap fun (Proxy @a) <> genericMap fun (Proxy @b)

instance (GQLType a, c a) => GFunctor c (M1 S s (K1 x a)) where
  genericMap Mappable {..} _ = mappableFun (KindedProxy :: KindedProxy (KIND a) a)

instance GFunctor c U1 where
  genericMap _ _ = mempty
