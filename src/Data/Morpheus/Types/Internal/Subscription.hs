{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE ScopedTypeVariables     #-}

module Data.Morpheus.Types.Internal.Subscription
  ( Client
  , connect
  , disconnect
  , toOutStream
  , runStreamWS
  , runStreamHTTP
  , Stream
  , Scope(..)
  , Input(..)
  , API(..)
  , acceptApolloRequest
  , publish
  , ClientStore
  )
where

import           Data.Foldable                  ( traverse_ )
import           Data.ByteString.Lazy.Char8     (ByteString)
import           Data.UUID.V4                   ( nextRandom )
import qualified Data.HashMap.Lazy   as   HM    ( insert
                                                , delete
                                                )

-- MORPHEUS
import           Data.Morpheus.Error.Utils      ( globalErrorMessage
                                                )
import           Data.Morpheus.Types.Internal.AST
                                                ( Value(..)
                                                , VALID
                                                , GQLErrors
                                                )
import           Data.Morpheus.Types.IO         ( GQLRequest(..) 
                                                , GQLResponse(..)
                                                )
import           Data.Morpheus.Types.Internal.Operation
                                                ( failure )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( SubEvent
                                                , GQLChannel(..)
                                                , ResponseEvent(..)
                                                , ResponseStream
                                                , runResultT
                                                , Result(..)
                                                , ResultT(..)
                                                )


-- MORPHEUS SUBSCRIPTION
import           Data.Morpheus.Types.Internal.Subscription.Apollo
                                                ( SubAction(..)
                                                , apolloFormat
                                                , toApolloResponse
                                                , acceptApolloRequest
                                                )
import           Data.Morpheus.Types.Internal.Subscription.ClientStore
                                                ( delete 
                                                , Client
                                                , publish
                                                , ClientStore
                                                )
import           Data.Morpheus.Types.Internal.Subscription.Stream
                                                ( toOutStream
                                                , runStreamWS
                                                , runStreamHTTP
                                                , Stream
                                                , Scope(..)
                                                , Input(..)
                                                , API(..)
                                                )
 
connect :: IO (Input 'Ws)
connect = Init <$> nextRandom

disconnect:: Scope 'Ws e m -> Input 'Ws -> m ()
disconnect WS { update }  (Init clientID)  = update (delete clientID)
