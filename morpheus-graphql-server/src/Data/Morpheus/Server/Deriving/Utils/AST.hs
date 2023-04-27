{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Morpheus.Server.Deriving.Utils.AST
  ( argumentsToObject,
    nodeToType,
    GQLTypeNode (..),
    DerivingMonad,
  )
where

import Control.Monad.Except (MonadError (..))
import Data.Morpheus.Types.Internal.AST
  ( Argument (..),
    Arguments,
    CONST,
    DirectiveDefinition (..),
    GQLError,
    Msg (..),
    ObjectEntry (..),
    TypeDefinition (..),
    VALID,
    Value (..),
  )

type DerivingMonad m = (MonadError GQLError m)

argumentsToObject :: Arguments VALID -> Value VALID
argumentsToObject = Object . fmap toEntry
  where
    toEntry Argument {..} = ObjectEntry argumentName argumentValue

data GQLTypeNode c
  = GQLTypeNode (TypeDefinition c CONST)
  | GQLDirectiveNode (DirectiveDefinition CONST)

nodeToType :: (Applicative f, MonadError GQLError f) => GQLTypeNode c -> f (TypeDefinition c CONST)
nodeToType node = case node of
  GQLTypeNode x -> pure x
  GQLDirectiveNode dir -> throwError $ "expected " <> msg (directiveDefinitionName dir) <> " to be a type but its directive!"
