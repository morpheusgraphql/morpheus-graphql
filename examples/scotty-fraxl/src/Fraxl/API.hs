{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Fraxl.API
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
    httpPlayground,
    runApp,
  )
import Data.Morpheus.Types
  ( GQLType,
    RootResolver (..),
    Undefined,
    defaultRootResolver,
    render,
  )
import Data.Set (Set, singleton)
import Data.Text (Text)
import qualified Fraxl.FakeDB as DB
import GHC.Generics (Generic)
import Web.Scotty
  ( ActionM,
    RoutePattern,
    ScottyM,
    body,
    get,
    param,
    post,
    raw,
  )
import Prelude
  ( Applicative (..),
    Int,
    Monad (..),
    String,
    flip,
    mempty,
    traverse,
    ($),
    (.),
    (<$>),
    (=<<),
  )

-- This file contains an example of how to use the `fraxl` library to address
-- the N+1 queries problem, a common problem with implementing GraphQL APIs.
--
-- We'll explain this problem more as we go, but first, let's define our GraphQL
-- API - here we have a database of bands, musicians, and instruments.  For the
-- purposes of this example, each band member is a member of a single band and
-- plays a single instrument.  The user can query for a list of all instruments
-- in the database.

newtype Query m = Query
  { instruments :: m [Instrument m]
  }
  deriving (Generic, GQLType)

data Instrument m = Instrument
  { name :: Text,
    players :: m [BandMember m]
  }
  deriving (Generic, GQLType)

data BandMember m = BandMember
  { name :: Text,
    band :: m (Band m)
  }
  deriving (Generic, GQLType)

data Band m = Band
  { name :: Text,
    members :: m [BandMember m]
  }
  deriving (Generic, GQLType)

-- Now, imagine we have a database structure with an `instrument` table, a
-- `band_memmber` table, and a `band` table.
--
-- To query all instruments, we would need to do a SELECT on the `instrument`
-- table.  If the graphql client asks for the `players` field of our
-- `Instrument` graphql type, we'll also have to get the band members associated
-- with each instrument.
--
-- One way to implement this would be to make a separate SELECT on the
-- `band_member` table for each instrument ID we receive from our initial SELECT
-- on the `instrument` table.  However, this will have bad performance with a
-- large number of instruments, since we'll have to send many queries to the
-- database.  This is a prototypical example of the N+1 queries problem.  While
-- there are many ways to achieve good performance, such as pre-fetching, the
-- `fraxl` library provides a particularly elegant solution to this issue.
--
-- To use `fraxl`, first we have to define a `Source` GADT that represents all
-- the different (unbatched) queries we can make to the database.

data Source a where
  GetInstruments :: Source [DB.Instrument]
  GetBandMemberByInstrumentID :: Int -> Source [DB.Member]
  GetBandMemberByBandID :: Int -> Source [DB.Member]
  GetBandByID :: Int -> Source DB.Band

-- Now, we define a type that represents a "batched" request, containing
-- potentially multiple different queries.

data BatchedReq = BatchedReq
  { memberInstrumentIDs :: Set Int,
    memberBandIDs :: Set Int,
    bandIDs :: Set Int
  }

-- Then, we define how to combine many `Source a` into a single `BatchedReq`.
-- Here `ASeq` is a heterogenous list type over `Source`.

batchSource :: ASeq Source a -> BatchedReq
batchSource = flip execState (BatchedReq mempty mempty mempty) . traverseASeq batch
  where
    batch :: Source a -> State BatchedReq (Source a)
    batch x@(GetBandMemberByInstrumentID y) = modify (\b -> b {memberInstrumentIDs = singleton y <> memberInstrumentIDs b}) >> pure x
    batch x@(GetBandMemberByBandID y) = modify (\b -> b {memberBandIDs = singleton y <> memberBandIDs b}) >> pure x
    batch x@(GetBandByID y) = modify (\b -> b {bandIDs = singleton y <> bandIDs b}) >> pure x
    batch x = pure x

-- Next, we define a result of the batched request.

data BatchedRes = BatchedRes
  { memberByInstrumentIDs :: Map Int [DB.Member],
    memberByBandIDs :: Map Int [DB.Member],
    bands :: Map Int DB.Band
  }

-- And, how to run the batched request to get a batched result - here `DB.Query`
-- is a monad that tracks the effect of accessing the database.

runBatch :: BatchedReq -> DB.Query BatchedRes
runBatch BatchedReq {memberInstrumentIDs, memberBandIDs, bandIDs} =
  BatchedRes
    <$> DB.getBandMembersByInstrumentID memberInstrumentIDs
      <*> DB.getBandMembersByBandID memberBandIDs
      <*> DB.getBandsByID bandIDs

-- This, `doFetch` helper actually provides the value for each `Source a`, while
-- reading from a `BatchedRes` result.

doFetch :: Source a -> Reader BatchedRes (DB.Query a)
doFetch GetInstruments = pure DB.getInstruments
doFetch (GetBandMemberByInstrumentID i) =
  asks (pure . fromJust . Data.Map.lookup i . memberByInstrumentIDs)
doFetch (GetBandMemberByBandID i) =
  asks (pure . fromJust . Data.Map.lookup i . memberByBandIDs)
doFetch (GetBandByID i) =
  asks (pure . fromJust . Data.Map.lookup i . bands)

-- We bundle these helpers together into a `fraxl` `Fetch`, which
-- interprets an `ASeq` of `Source a`s under the `DB.Query` monad.  This is
-- where the power of fraxl lies - while handling a GraphQL request, this
-- `fetch` will be invoked multiple times, each time with a collection of
-- `Source a` requests that do not depend on eachother. This allows us to use
-- `batchSource` to combine a large number of `Source`s into a single database query.
--
-- For more on how this works, see the `fraxl` documentation or documentation on
-- `Haxl`, the library that inspired fraxl.

fetchSource :: Fetch Source DB.Query a
fetchSource as = runReader (traverseASeq doFetch as) <$> runBatch (batchSource as)

-- Now, we write our QraphQL resolvers, using `fraxl`'s `dataFetch` with one of
-- our `Source` constructors whenever we need to access the DB.

resolveBand ::
  (Monad (t (FreerT Source m)), MonadTrans t, Monad m) =>
  DB.Band ->
  t (FreerT Source m) (Band (t (FreerT Source m)))
resolveBand DB.Band {name, id = i} =
  pure
    Band
      { name,
        members = lift (dataFetch $ GetBandMemberByBandID i) >>= traverse resolveBandMember
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
        players = lift (dataFetch $ GetBandMemberByInstrumentID i) >>= traverse resolveBandMember
      }

resolveInstruments :: (Monad (t (FreerT Source m)), MonadTrans t, Monad m) => t (FreerT Source m) [Instrument (t (FreerT Source m))]
resolveInstruments = lift (dataFetch GetInstruments) >>= traverse resolveInstrument

rootResolver :: (Monad m) => RootResolver (FreerT Source m) () Query Undefined Undefined
rootResolver =
  defaultRootResolver
    { queryResolver = Query {instruments = resolveInstruments}
    }

app :: App () (FreerT Source DB.Query)
app = deriveApp rootResolver

-- Finally, we can `runFraxl` with our `fetchSource` fetcher.

isSchema :: ActionM String
isSchema = param "schema"

httpEndpoint :: RoutePattern -> ScottyM ()
httpEndpoint route = do
  get route $
    (isSchema *> raw (render app))
      <|> raw httpPlayground
  post route (raw . DB.runQuery . runFraxl fetchSource . runApp app =<< body)
