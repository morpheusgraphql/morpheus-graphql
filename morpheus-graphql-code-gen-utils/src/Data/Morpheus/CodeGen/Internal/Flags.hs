{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Morpheus.CodeGen.Internal.Flags
  ( Flags,
    Flag (..),
    runCodeGenT,
    CodeGenT,
    langExtension,
    requireExternal,
  )
where

import Control.Monad.Except
import Data.Morpheus.Types.Internal.AST (GQLError)
import Data.Text
import Relude hiding (ByteString, get)

type Flags = [Flag]

data Flag
  = FlagLanguageExtension Text
  | FlagExternal Text
  deriving (Ord, Eq, Show)

newtype CodeGenT ctx (m :: Type -> Type) a = CodeGenT
  { _runCodeGenT :: ReaderT ctx (StateT Flags m) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadFail,
      MonadReader ctx,
      MonadState
        Flags
    )

deriving instance (MonadError GQLError m) => MonadError GQLError (CodeGenT ctx m)

instance MonadTrans (CodeGenT ctx) where
  lift = CodeGenT . lift . lift

runCodeGenT :: (Monad m) => CodeGenT ctx m a -> ctx -> m (a, Flags)
runCodeGenT (CodeGenT m) ctx = runStateT (runReaderT m ctx) mempty

langExtension :: (MonadState Flags m) => Text -> m ()
langExtension ext = modify (FlagLanguageExtension ext :)

requireExternal :: (MonadState Flags m) => Text -> m ()
requireExternal ext = modify (FlagExternal ext :)
