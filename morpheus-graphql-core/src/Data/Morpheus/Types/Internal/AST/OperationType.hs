{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Types.Internal.AST.OperationType
  ( OperationType (..),
    QUERY,
    MUTATION,
    SUBSCRIPTION,
    toOperationType,
    isOperationType,
  )
where

import Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL (..),
  )
import Data.Morpheus.Types.Internal.AST.Error (Msg (..))
import Data.Morpheus.Types.Internal.AST.Name (TypeName)
import Language.Haskell.TH.Syntax
  ( Lift,
  )
import Relude hiding
  ( ByteString,
    Show,
    decodeUtf8,
    intercalate,
    show,
  )
import Prelude (Show (..))

type QUERY = 'OPERATION_QUERY

type MUTATION = 'OPERATION_MUTATION

type SUBSCRIPTION = 'OPERATION_SUBSCRIPTION

data OperationType
  = OPERATION_QUERY
  | OPERATION_SUBSCRIPTION
  | OPERATION_MUTATION
  deriving
    ( Eq,
      Lift,
      Generic,
      Hashable,
      Show
    )

instance RenderGQL OperationType where
  renderGQL OPERATION_QUERY = "query"
  renderGQL OPERATION_MUTATION = "mutation"
  renderGQL OPERATION_SUBSCRIPTION = "subscription"

isOperationType :: OperationType -> TypeName -> Bool
isOperationType OPERATION_QUERY "Query" = True
isOperationType OPERATION_MUTATION "Mutation" = True
isOperationType OPERATION_SUBSCRIPTION "Subscription" = True
isOperationType _ _ = False
{-# INLINE isOperationType #-}

toOperationType :: TypeName -> Maybe OperationType
toOperationType "Subscription" = Just OPERATION_SUBSCRIPTION
toOperationType "Mutation" = Just OPERATION_MUTATION
toOperationType "Query" = Just OPERATION_QUERY
toOperationType _ = Nothing
{-# INLINE toOperationType #-}

instance Msg OperationType where
  msg OPERATION_QUERY = msg ("query" :: TypeName)
  msg OPERATION_MUTATION = msg ("mutation" :: TypeName)
  msg OPERATION_SUBSCRIPTION = msg ("subscription" :: TypeName)
