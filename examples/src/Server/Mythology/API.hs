{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Server.Mythology.API
  ( mythologyApi,
    mythologyRoot
  )
where

import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Morpheus                  ( interpreter )
import           Data.Morpheus.Types            ( GQLRootResolver(..)
                                                , GQLType
                                                , IORes
                                                , Undefined(..)
                                                , liftEither
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Server.Mythology.Character     ( Deity
                                                , Human
                                                , dbDeity
                                                , someHuman
                                                , someDeity
                                                )
import           Server.Mythology.Place         ( City )


data Character m =
    CharacterHuman (Human m) -- Only <tyconName><conName> should generate direct link
  | CharacterDeity Deity -- Only <tyconName><conName> should generate direct link
  -- RECORDS
  | Creature { name :: Text, age :: Int }
  | BoxedDeity { boxedDeity :: Deity}
  | SomeScalarRecord { scalar :: Text }
  --- Types 
  | SomeDeity Deity
  | SomeScalar Int
  | SomeMutli Int Text
  --- ENUMS
  | Zeus
  | Cronus deriving (Generic, GQLType)

data Query m = Query
  { deity :: DeityArgs -> m Deity,
    character :: [Character m]
  } deriving (Generic, GQLType)

data DeityArgs = DeityArgs
  { name      :: Text -- Required Argument
  , bornPlace :: Maybe City -- Optional Argument
  } deriving (Generic)

resolveDeity :: DeityArgs -> IORes e Deity
resolveDeity DeityArgs { name, bornPlace } =
  liftEither $ dbDeity name bornPlace

resolveCharacter :: Applicative m => [Character m]
resolveCharacter =
  [ CharacterHuman someHuman
  , CharacterDeity someDeity
  , Creature { name = "Lamia", age = 205 }
  , BoxedDeity { boxedDeity = someDeity }
  , SomeScalarRecord { scalar = "Some Text" }
  ---
  , SomeDeity someDeity
  , SomeScalar 12
  , SomeMutli 21 "some text"
  , Zeus
  , Cronus
  ]

mythologyRoot :: GQLRootResolver IO () Query Undefined Undefined
mythologyRoot = GQLRootResolver
  { queryResolver = Query { deity = resolveDeity, character = resolveCharacter }
  , mutationResolver = Undefined
  , subscriptionResolver = Undefined
  }

mythologyApi :: B.ByteString -> IO B.ByteString
mythologyApi = interpreter mythologyRoot
