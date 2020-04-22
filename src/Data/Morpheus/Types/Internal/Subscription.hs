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

import           Data.UUID.V4                   ( nextRandom )

-- MORPHEUS SUBSCRIPTION
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