{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.Refs
  ( RefScanner (..),
  )
where

import Data.Morpheus.App.Internal.Resolving.ResolverState
  ( ResolverContext (..),
    askFieldTypeName,
    updateCurrentType,
  )
import Data.Morpheus.App.Internal.Resolving.Types (NamedResolverRef, ObjectTypeResolver (..), ResolverValue (..))
import Data.Morpheus.App.Internal.Resolving.Utils (ResolverMonad, withField, withObject)
import Data.Morpheus.Types.Internal.AST
  ( Selection (..),
    SelectionContent (..),
    SelectionSet,
    VALID,
  )
import Relude hiding (empty)

type SelectionRef = (SelectionContent VALID, NamedResolverRef)

class RefScanner f where
  type RefSel f :: Type
  scanRefs :: (ResolverMonad m) => RefSel f -> f m -> m [SelectionRef]

instance RefScanner ResolverValue where
  type RefSel ResolverValue = SelectionContent VALID
  scanRefs sel (ResList xs) = concat <$> traverse (scanRefs sel) xs
  scanRefs sel (ResLazy x) = x >>= scanRefs sel
  scanRefs sel (ResObject tyName obj) = withObject tyName (`scanRefs` obj) sel
  scanRefs sel (ResRef ref) = pure . (sel,) <$> ref
  scanRefs _ ResEnum {} = pure []
  scanRefs _ ResNull = pure []
  scanRefs _ ResScalar {} = pure []

instance RefScanner ObjectTypeResolver where
  type RefSel ObjectTypeResolver = Maybe (SelectionSet VALID)
  scanRefs Nothing _ = pure []
  scanRefs (Just sel) objRes = concat <$> traverse fieldRefs (toList sel)
    where
      fieldRefs currentSelection@Selection {..}
        | selectionName == "__typename" = pure []
        | otherwise = do
            t <- askFieldTypeName selectionName
            updateCurrentType t $
              local (\ctx -> ctx {currentSelection}) $ do
                x <- withField [] (fmap pure) selectionName objRes
                concat <$> traverse (scanRefs selectionContent) x
