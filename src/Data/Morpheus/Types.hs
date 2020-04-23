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
  , Resolver
  , QUERY
  , MUTATION
  , SUBSCRIPTION
  , lift
  , liftEither
  , ResolveQ
  , ResolveM
  , ResolveS
  , failRes
  , WithOperation
  , publish
  , subscribe
  , unsafeInternalContext
  , SubField
  , Input
  , Stream
  , Store(..)
  , initDefaultStore
  , publishEventWith
  )
where

import           Data.Text                      ( pack )
import           Data.Either                    (either)
import           Control.Monad.Trans.Class      ( MonadTrans(..) )

-- MORPHEUS
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
                                                , Message
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Event(..)
                                                , GQLRootResolver(..)
                                                , Resolver
                                                , WithOperation
                                                , lift
                                                , failure
                                                , Failure
                                                , pushEvents
                                                , PushEvents(..)
                                                , subscribe
                                                , unsafeInternalContext
                                                , UnSubResolver
                                                )
import           Data.Morpheus.Types.IO         ( GQLRequest(..)
                                                , GQLResponse(..)
                                                )
import           Data.Morpheus.Types.Types      ( Undefined(..) )
import           Data.Morpheus.Types.Internal.Subscription  
                                                ( Stream
                                                , Input 
                                                , Store(..)
                                                , initDefaultStore 
                                                , publishEventWith
                                                )

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

-- Subsciption Object Resolver Fields
type SubField m a = (m (a (UnSubResolver m))) 

publish :: Monad m => [e] -> Resolver MUTATION e m ()
publish = pushEvents

-- resolves constant value on any argument
constRes :: (WithOperation o, Monad m) => b -> a -> Resolver o e m b
constRes = const . pure

constMutRes :: Monad m => [e] -> a -> args -> MutRes e m a
constMutRes events value = const $ do 
  publish events  
  pure value

failRes :: (WithOperation o, Monad m) => String -> Resolver o e m a
failRes = failure . pack

liftEither :: (MonadTrans t, Monad (t m), Failure Message (t m)) => Monad m => m (Either String a) -> t m a
liftEither x = lift x >>= either (failure . pack) pure