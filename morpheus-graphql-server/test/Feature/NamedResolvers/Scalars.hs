{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Feature.NamedResolvers.Scalars
  ( Doc,
  )
where

import Data.Morpheus.Server.Resolvers
  ( ResolveNamed (..),
  )
import Data.Morpheus.Server.Types
  ( DecodeScalar (..),
    EncodeScalar (..),
    GQLType (..),
    ID,
    SCALAR,
  )
import Data.Text (Text)
import Feature.NamedResolvers.DB
  ( getDocsById,
  )
import Feature.NamedResolvers.RealmsApp (Deity, Realm)
import GHC.Generics (Generic)

newtype Doc = Doc Text
  deriving newtype
    ( DecodeScalar,
      EncodeScalar
    )

instance GQLType Doc where
  type KIND Doc = SCALAR

instance ResolveNamed m Doc where
  type Dep Doc = ID
  resolveBatched = traverse (fmap (fmap Doc) . getDocsById)

-- Entity
data Entity m
  = EntityDeity (m (Deity m))
  | EntityRealm (m (Realm m))
  deriving
    ( Generic,
      GQLType
    )
