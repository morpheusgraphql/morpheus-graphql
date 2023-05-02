{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Internal.Decode.Utils
  ( repValue,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Morpheus.Generic
  ( GRepField (..),
    GRepValue (..),
  )
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Internal.Utils
  ( fromElems,
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    ObjectEntry (..),
    Value (..),
    internal,
  )
import Relude

repValue :: GRepValue (GQLResult (Value CONST)) -> GQLResult (Value CONST)
repValue GRepValueEnum {..} = pure $ Enum enumVariantName
repValue GRepValueObject {..} = Object <$> (traverse fromField objectFields >>= fromElems)
  where
    fromField GRepField {fieldSelector, fieldValue} = do
      entryValue <- fieldValue
      pure ObjectEntry {entryName = fieldSelector, entryValue}
repValue _ = throwError (internal "input unions are not supported")