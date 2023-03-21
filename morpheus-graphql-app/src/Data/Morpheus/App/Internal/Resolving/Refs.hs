{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.App.Internal.Resolving.Refs
  ( scanRefs,
  )
where

import Data.Morpheus.App.Internal.Resolving.ResolverState
  ( inSelectionField,
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

scanRefs :: (ResolverMonad m) => SelectionContent VALID -> ResolverValue m -> m [SelectionRef]
scanRefs sel (ResList xs) = concat <$> traverse (scanRefs sel) xs
scanRefs sel (ResLazy x) = x >>= scanRefs sel
scanRefs sel (ResObject tyName obj) = withObject tyName (objectRefs obj) sel
scanRefs sel (ResRef ref) = pure . (sel,) <$> ref
scanRefs _ ResEnum {} = pure []
scanRefs _ ResNull = pure []
scanRefs _ ResScalar {} = pure []

objectRefs :: (ResolverMonad m) => ObjectTypeResolver m -> Maybe (SelectionSet VALID) -> m [SelectionRef]
objectRefs _ Nothing = pure []
objectRefs obj (Just sel) = concat <$> traverse (fieldRefs obj) (toList sel)

fieldRefs :: (ResolverMonad m) => ObjectTypeResolver m -> Selection VALID -> m [SelectionRef]
fieldRefs obj selection@Selection {..}
  | selectionName == "__typename" = pure []
  | otherwise = inSelectionField selection $ do
      resValue <- withField [] (fmap pure) selectionName obj
      concat <$> traverse (scanRefs selectionContent) resValue
