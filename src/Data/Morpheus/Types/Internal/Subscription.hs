{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE KindSignatures   #-}

module Data.Morpheus.Types.Internal.Subscription
  ( Client(..)
  , ID
  , PubSubStore
  , SesionID
  , unfold
  , empty
  , insert
  , adjust
  , delete
  , concatUnfold
  )
where

import           Data.ByteString.Lazy.Char8     (ByteString)
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import           Data.UUID                      ( UUID )
import           Data.HashMap.Lazy              ( HashMap , keys)
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
    { clientId         :: ID
    , clientConnection :: ByteString -> m ()
    , clientSessions   :: HashMap SesionID (SubEvent e m)
    }

instance Show (Client ref e m) where
  show Client { clientId, clientSessions } =
    "Client { id: "
      <> show clientId
      <> ", sessions: "
      <> show (keys clientSessions)
      <> " }"

-- subscription 
-- store
-- stores active subsciption connections and requests
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
  :: ID
  -> (ByteString -> m ())
  -> StoreMap ref e m
insert clientId clientConnection = mapStore (HM.insert clientId  c)
  where
    c = Client { clientId , clientConnection, clientSessions = HM.empty }

adjust 
  :: (Client ref e m -> Client ref e m ) 
  -> ID
  -> StoreMap ref e m
adjust f key = mapStore (HM.adjust f key)

delete 
  :: ID
  -> StoreMap ref e m
delete key = mapStore (HM.delete key)