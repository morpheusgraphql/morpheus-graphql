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
  )
where

import Data.Char (toLower)
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
    decodeUtf8,
    intercalate,
  )

type QUERY = 'Query

type MUTATION = 'Mutation

type SUBSCRIPTION = 'Subscription

data OperationType
  = Query
  | Subscription
  | Mutation
  deriving
    ( Show,
      Eq,
      Lift,
      Generic,
      Hashable
    )

instance RenderGQL OperationType where
  renderGQL = fromString . fmap toLower . show

toOperationType :: TypeName -> Maybe OperationType
toOperationType "Subscription" = Just Subscription
toOperationType "Mutation" = Just Mutation
toOperationType "Query" = Just Query
toOperationType _ = Nothing
{-# INLINE toOperationType #-}

instance Msg OperationType where
  msg Query = msg ("query" :: TypeName)
  msg Mutation = msg ("mutation" :: TypeName)
  msg Subscription = msg ("subscription" :: TypeName)
