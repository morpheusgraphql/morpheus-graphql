{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE KindSignatures   #-}

module Data.Morpheus.Types.Internal.Subscription
  ( Client(..)
  , ID
  , PubSubStore
  , SesionID
  , empty
  , insert
  , adjust
  , delete
  , elems
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

data Client e ( m :: * -> * ) =
  Client
    { clientId       :: ID
    , clientCallback :: ByteString -> m ()
    , clientSessions :: HashMap SesionID (SubEvent e m)
    }

instance Show (Client e m) where
  show Client { clientId, clientSessions } =
    "Client { id: "
      <> show clientId
      <> ", sessions: "
      <> show (keys clientSessions)
      <> " }"

-- subscription 
-- store
-- stores active subsciption connections and requests
newtype PubSubStore e ( m :: * -> * ) = 
    PubSubStore 
      { unpackStore :: HashMap ID (Client e m)
      } deriving (Show)

type StoreMap e m
  = PubSubStore e m 
  -> PubSubStore e m

mapStore 
  ::  ( HashMap ID (Client e m) 
      -> HashMap ID (Client e m)
      )
  -> StoreMap e m
mapStore f = PubSubStore . f . unpackStore

elems :: PubSubStore e m ->  [Client e m]
elems = HM.elems . unpackStore

instance Empty (PubSubStore e m) where
  empty = PubSubStore HM.empty

insert 
  :: ID
  -> (ByteString -> m ())
  -> StoreMap e m
insert clientId clientCallback = mapStore (HM.insert clientId  c)
  where
    c = Client { clientId , clientCallback, clientSessions = HM.empty }

adjust 
  :: (Client e m -> Client e m ) 
  -> ID
  -> StoreMap e m
adjust f key = mapStore (HM.adjust f key)

delete 
  :: ID
  -> StoreMap e m
delete key = mapStore (HM.delete key)