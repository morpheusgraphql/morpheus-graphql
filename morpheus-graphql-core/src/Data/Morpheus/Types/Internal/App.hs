{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Types.Internal.App
  ( App (..),
    AppNode (..),
  )
where

import Data.Morpheus.Internal.Utils (prop)
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    GQLErrors,
    Schema (..),
  )
import Data.Morpheus.Types.Internal.Resolving (RootResModel, resultOr)
import Data.Morpheus.Types.Internal.Stitching (Stitching (..))

data App event (m :: * -> *)
  = App {appNode :: AppNode event m}
  | FailApp {appErrors :: GQLErrors}

instance Semigroup (App e m) where
  (FailApp err1) <> (FailApp err2) = FailApp (err1 <> err2)
  FailApp {appErrors} <> App {} = FailApp appErrors
  App {} <> FailApp {appErrors} = FailApp appErrors
  (App x) <> (App y) = resultOr FailApp App $ stitch x y

data AppNode event (m :: * -> *) = AppNode
  { appResolvers :: RootResModel event m,
    appSchema :: Schema CONST
  }

instance Stitching (AppNode e m) where
  stitch x y =
    AppNode
      <$> prop stitch appResolvers x y
      <*> prop stitch appSchema x y
