{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Fraxl.API
  ( httpEndpoint,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Fraxl (ASeq (..), Fetch, FreerT, dataFetch, runFraxl, traverseASeq)
import Control.Monad.Reader (Reader, asks, runReader)
import Control.Monad.State (State, execState, modify)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Map (Map, lookup)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Morpheus
  ( App,
    deriveApp,
    runApp,
  )
import Data.Morpheus.Server
  ( httpPlayground,
  )
import Data.Morpheus.Types
  ( GQLType,
    RootResolver (..),
    Undefined (..),
    render,
  )
import Data.Set (Set, singleton)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Server.Fraxl.FakeDB as DB
import Server.Utils (isSchema)
import Web.Scotty
  ( RoutePattern,
    ScottyM,
    body,
    get,
    post,
    raw,
  )

data Instrument m
  = Instrument
      { name :: Text,
        players :: m [BandMember m]
      }
  deriving (Generic, GQLType)

data BandMember m
  = BandMember
      { name :: Text,
        band :: m (Band m)
      }
  deriving (Generic, GQLType)

data Band m
  = Band
      { name :: Text,
        members :: m [BandMember m]
      }
  deriving (Generic, GQLType)

data Source a where
  GetInstruments :: Source [DB.Instrument]
  GetBandMemberByInstrumentID :: Int -> Source [DB.Member]
  GetBandMemberByBandID :: Int -> Source [DB.Member]
  GetBandByID :: Int -> Source DB.Band

data BatchedReq
  = BatchedReq
      { memberInstrumentIDs :: Set Int,
        memberBandIDs :: Set Int,
        bandIDs :: Set Int
      }

data BatchedRes
  = BatchedRes
      { memberByInstrumentIDs :: Map Int [DB.Member],
        memberByBandIDs :: Map Int [DB.Member],
        bands :: Map Int DB.Band
      }

batchSource :: ASeq Source a -> BatchedReq
batchSource = flip execState (BatchedReq mempty mempty mempty) . traverseASeq batch
  where
    batch :: Source a -> State BatchedReq (Source a)
    batch x@(GetBandMemberByInstrumentID y) = modify (\b -> b {memberInstrumentIDs = singleton y <> memberInstrumentIDs b}) >> pure x
    batch x@(GetBandMemberByBandID y) = modify (\b -> b {memberBandIDs = singleton y <> memberBandIDs b}) >> pure x
    batch x@(GetBandByID y) = modify (\b -> b {bandIDs = singleton y <> bandIDs b}) >> pure x
    batch x = pure x

runBatch :: BatchedReq -> DB.Query BatchedRes
runBatch BatchedReq {memberInstrumentIDs, memberBandIDs, bandIDs} =
  BatchedRes
    <$> DB.getBandMembersByInstrumentID memberInstrumentIDs
    <*> DB.getBandMembersByBandID memberBandIDs
    <*> DB.getBandsByID bandIDs

doFetch :: Source a -> Reader BatchedRes (DB.Query a)
doFetch GetInstruments = pure DB.getInstruments
doFetch (GetBandMemberByInstrumentID i) =
  asks (pure . fromJust . Data.Map.lookup i . memberByInstrumentIDs)
doFetch (GetBandMemberByBandID i) =
  asks (pure . fromJust . Data.Map.lookup i . memberByBandIDs)
doFetch (GetBandByID i) =
  asks (pure . fromJust . Data.Map.lookup i . bands)

fetchSource :: Fetch Source DB.Query a
fetchSource as = runReader (traverseASeq doFetch as) <$> runBatch (batchSource as)

newtype Query m
  = Query
      { instruments :: m [Instrument m]
      }
  deriving (Generic, GQLType)

resolveBand ::
  (Monad (t (FreerT Source m)), MonadTrans t, Monad m) =>
  DB.Band ->
  t (FreerT Source m) (Band (t (FreerT Source m)))
resolveBand DB.Band {name, id = i} =
  pure
    Band
      { name,
        members = lift (dataFetch $ GetBandMemberByBandID i) >>= sequenceA . (resolveBandMember <$>)
      }

resolveBandMember ::
  (Monad (t (FreerT Source m)), MonadTrans t, Monad m) =>
  DB.Member ->
  t (FreerT Source m) (BandMember (t (FreerT Source m)))
resolveBandMember DB.Member {name, bandID} =
  pure
    BandMember
      { name,
        band = lift (dataFetch $ GetBandByID bandID) >>= resolveBand
      }

resolveInstrument ::
  (Monad (t (FreerT Source m)), MonadTrans t, Monad m) =>
  DB.Instrument ->
  t (FreerT Source m) (Instrument (t (FreerT Source m)))
resolveInstrument DB.Instrument {name, id = i} =
  pure
    Instrument
      { name,
        players = lift (dataFetch $ GetBandMemberByInstrumentID i) >>= sequenceA . (resolveBandMember <$>)
      }

resolveInstruments :: (Monad (t (FreerT Source m)), MonadTrans t, Monad m) => t (FreerT Source m) [Instrument (t (FreerT Source m))]
resolveInstruments = lift (dataFetch GetInstruments) >>= sequenceA . (resolveInstrument <$>)

rootResolver :: (Monad m) => RootResolver (FreerT Source m) () Query Undefined Undefined
rootResolver =
  RootResolver
    { queryResolver = Query {instruments = resolveInstruments},
      mutationResolver = Undefined,
      subscriptionResolver = Undefined
    }

app :: App () (FreerT Source DB.Query)
app = deriveApp rootResolver

httpEndpoint ::
  RoutePattern ->
  ScottyM ()
httpEndpoint route = do
  get route $
    (isSchema *> raw (render app))
      <|> raw httpPlayground
  post route $ raw =<< ((pure . DB.runQuery . runFraxl fetchSource . runApp app) =<< body)
