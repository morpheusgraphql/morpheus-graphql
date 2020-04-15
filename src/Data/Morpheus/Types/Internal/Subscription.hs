{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE KindSignatures   #-}

module Data.Morpheus.Types.Internal.Subscription
  ( Client(..)
  , ID
  , PubSubStore
  , GQLState
  , SesionID
  , unfold
  , empty
  , insert
  , adjust
  , delete
  , concatUnfold
  )
where

import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )
import           Data.HashMap.Lazy              ( HashMap , keys)
import           Control.Concurrent             ( MVar )
import qualified Data.HashMap.Lazy   as  HM     ( empty
                                                , insert
                                                , delete
                                                , adjust
                                                , elems
                                                )
-- MORPHEUS
import           Data.Morpheus.Types.Internal.Operation
                                                (Empty(..))
import           Data.Morpheus.Types.Internal.Resolving
                                                ( SubEvent )



type ID = UUID

type SesionID = Text

data Client ref e ( m :: * -> * ) =
  Client
    { clientID         :: ID
    , clientConnection :: ref
    , clientSessions   :: HashMap SesionID (SubEvent e m)
    }

instance Show (Client ref e m) where
  show Client { clientID, clientSessions } =
    "GQLClient "
      <>"{ id: "
      <> show clientID
      <> ", sessions: "
      <> show (keys clientSessions)
      <> " }"


newtype PubSubStore ref e ( m :: * -> * ) = 
    PubSubStore 
      { runPubSubStore :: HashMap ID (Client ref e m)
      } deriving (Show)

type StoreMap ref e m
  = PubSubStore ref e m 
  -> PubSubStore ref e m

mapStore 
  ::  ( HashMap ID (Client ref e m) 
      -> HashMap ID (Client ref e m)
      )
  -> StoreMap ref e m
mapStore f = PubSubStore . f . runPubSubStore

unfold :: (Client ref e m -> a)-> PubSubStore ref e m ->  [a]
unfold f = map f . HM.elems . runPubSubStore

concatUnfold :: (Client ref e m -> [a])-> PubSubStore ref e m ->  [a]
concatUnfold f = concat . unfold f

instance Empty (PubSubStore ref e m) where
  empty = PubSubStore HM.empty

insert 
  :: Client ref e m 
  -> StoreMap ref e m
insert c = mapStore (HM.insert (clientID c) c)

adjust 
  :: (Client ref e m -> Client ref e m ) 
  -> ID
  -> StoreMap ref e m
adjust f key = mapStore (HM.adjust f key)

delete 
  :: ID
  -> StoreMap ref e m
delete key = mapStore (HM.delete key)


-- | shared GraphQL state between __websocket__ and __http__ server,
-- stores information about subscriptions
type GQLState ref e m = MVar (PubSubStore ref e m) -- SharedState