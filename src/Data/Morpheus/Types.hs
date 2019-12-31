{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE ConstraintKinds #-}
-- | GQL Types
module Data.Morpheus.Types
  ( Event(..)
  -- Type Classes
  , GQLType(KIND, description)
  , GQLScalar(parseValue, serialize)
  , GQLRequest(..)
  , GQLResponse(..)
  , ID(..)
  , ScalarValue(..)
  , GQLRootResolver(..)
  , constRes
  , constMutRes
  , Undefined(..)
  , Res
  , MutRes
  , SubRes
  , IORes
  , IOMutRes
  , IOSubRes
  , Resolver(..)
  , QUERY
  , MUTATION
  , SUBSCRIPTION
  , liftEither
  , lift
  , ResolveQ
  , ResolveM
  , ResolveS
  , failRes
  , WithOperation
  )
where

import           Data.Text                      ( pack )
import           Data.Morpheus.Types.GQLScalar  ( GQLScalar
                                                  ( parseValue
                                                  , serialize
                                                  )
                                                )
import           Data.Morpheus.Types.GQLType    ( GQLType(KIND, description) )
import           Data.Morpheus.Types.ID         ( ID(..) )
import           Data.Morpheus.Types.Internal.AST
                                                ( MUTATION
                                                , QUERY
                                                , SUBSCRIPTION
                                                , ScalarValue(..)
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Event(..)
                                                , GQLRootResolver(..)
                                                , Resolver(..)
                                                , WithOperation
                                                , lift
                                                , liftEither
                                                , failure
                                                )
import           Data.Morpheus.Types.IO         ( GQLRequest(..)
                                                , GQLResponse(..)
                                                )
import           Data.Morpheus.Types.Types      ( Undefined(..) )

type Res = Resolver QUERY
type MutRes = Resolver MUTATION
type SubRes = Resolver SUBSCRIPTION

type IORes e = Res e IO
type IOMutRes e = MutRes e IO
type IOSubRes e = SubRes e IO

-- Recursive Resolvers
type ResolveQ e m a = Res e m (a (Res e m))
type ResolveM e m a = MutRes e m (a (MutRes e m))
type ResolveS e m a = SubRes e m (a (Res e m))

-- resolves constant value on any argument
constRes :: (WithOperation o, Monad m) => b -> a -> Resolver o e m b
constRes = const . pure

constMutRes :: Monad m => [e] -> a -> args -> MutRes e m a
constMutRes events value = const $ MutResolver $ pure (events, value)

failRes :: (WithOperation o, Monad m) => String -> Resolver o e m a
failRes = failure . pack
