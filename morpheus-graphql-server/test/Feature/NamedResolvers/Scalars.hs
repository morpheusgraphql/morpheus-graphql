{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.NamedResolvers.Scalars
  ( Markdown,
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

newtype Markdown = Markdown Text
  deriving newtype
    ( DecodeScalar,
      EncodeScalar
    )

instance GQLType Markdown where
  type KIND Markdown = SCALAR

instance ResolveNamed m Markdown where
  type Dep Markdown = ID
  resolveBatched = traverse (fmap (fmap Markdown) . getDocsById)
