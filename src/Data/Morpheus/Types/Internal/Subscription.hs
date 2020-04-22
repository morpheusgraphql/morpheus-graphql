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
  , Store(..)
  , initDefaultStore
  )
where

import           Control.Concurrent             ( readMVar
                                                , newMVar
                                                , modifyMVar_
                                                )
import           Data.UUID.V4                   ( nextRandom )

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Operation
                                                (empty)
import           Control.Monad.IO.Class         ( MonadIO(..) )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( GQLChannel(..) )
import           Data.Morpheus.Types.Internal.Subscription.Apollo
                                                ( acceptApolloRequest )
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


-- | shared GraphQL state between __websocket__ and __http__ server,
-- stores information about subscriptions
data Store e m = Store 
  { readStore :: m (ClientStore e m)
  , writeStore :: (ClientStore e m -> ClientStore e m) -> m ()
  }

-- | initializes empty GraphQL state
initDefaultStore :: 
  ( MonadIO m
  , (Eq (StreamChannel event)) 
  , (GQLChannel event) 
  ) 
  => IO (Store event m)
initDefaultStore = do
  store <- newMVar empty
  pure Store 
    { readStore = liftIO $ readMVar store
    , writeStore = \changes -> liftIO $ modifyMVar_ store (return . changes)
    }
