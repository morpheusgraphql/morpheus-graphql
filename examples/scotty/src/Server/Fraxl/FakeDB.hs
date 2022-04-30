{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Fraxl.FakeDB
  ( Band (..),
    Member (..),
    Instrument (..),
    Query,
    runQuery,
    getInstruments,
    getBandMembersByInstrumentID,
    getBandMembersByBandID,
    getBandsByID,
  )
where

import Data.Foldable (fold)
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set, toList)
import Data.Text (Text)

data Band = Band
  { name :: Text,
    id :: Int
  }

bandDB :: [Band]
bandDB =
  [ Band "Jem and the Holograms" 0,
    Band "The Misfits" 1
  ]

data Member = Member
  { name :: Text,
    instrumentID :: Int,
    bandID :: Int
  }

memberDB :: [Member]
memberDB =
  [ Member "Jerrica" 0 0,
    Member "Kimber" 1 0,
    Member "Aja" 2 0,
    Member "Shana" 3 0,
    Member "Carmen" 4 0,
    Member "Glenn" 0 1,
    Member "Franché" 2 1,
    Member "Jerry" 3 1,
    Member "Jim" 4 1
  ]

data Instrument = Instrument
  { name :: Text,
    id :: Int
  }

instrumentDB :: [Instrument]
instrumentDB =
  [ Instrument "Vocals" 0,
    Instrument "Synthesizer" 1,
    Instrument "Guitar" 2,
    Instrument "Bass Guitar" 3,
    Instrument "Drums" 4
  ]

newtype Query a = Query (Identity a) deriving (Functor, Applicative, Monad)

runQuery :: Query a -> a
runQuery (Query a) = runIdentity a

getInstruments :: Query [Instrument]
getInstruments = pure instrumentDB

lookupOneToMany :: (Ord b) => (a -> b) -> [a] -> Set b -> Map b [a]
lookupOneToMany x ys xs =
  foldMap
    (\x' -> M.singleton x' (filter (\y -> x y == x') ys))
    (toList xs)

lookupOneToOne :: (Ord b) => (a -> b) -> [a] -> Set b -> Map b a
lookupOneToOne x ys xs =
  foldMap
    (\y -> M.singleton (x y) y)
    (filter (\y -> x y `elem` xs) ys)

getBandMembersByInstrumentID :: Set Int -> Query (Map Int [Member])
getBandMembersByInstrumentID = pure . lookupOneToMany instrumentID memberDB

getBandMembersByBandID :: Set Int -> Query (Map Int [Member])
getBandMembersByBandID = pure . lookupOneToMany bandID memberDB

getBandsByID :: Set Int -> Query (Map Int Band)
getBandsByID = pure . lookupOneToOne (\Band {id = i} -> i) bandDB
