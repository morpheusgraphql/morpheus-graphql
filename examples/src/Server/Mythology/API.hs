{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.Mythology.API
  ( mythologyApi
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
import           Server.Mythology.Character.Deity
                                                ( Deity(..)
                                                , dbDeity
                                                )
import qualified Server.Mythology.Character.Human
                                               as H
                                                ( Human(..) )
import           Server.Mythology.Place.Places  ( City(..)
                                                , Realm(..)
                                                )


data Character  =
    CharacterHuman H.Human -- Only <tyconName><conName> should generate direct link
  | CharacterDeity Deity -- Only <tyconName><conName> should generate direct link
  -- RECORDS
  | Creature { creatureName :: Text, creatureAge :: Int }
  | BoxedDeity { boxedDeity :: Deity}
  | SomeScalarRecord { scalarText :: Text }
  --- Types 
  | SomeDeity Deity
  | SomeScalar Int
  | SomeMutli Int Text
  --- ENUMS
  | Zeus
  | Cronus deriving (Generic, GQLType)

data Query m = Query
  { deity :: DeityArgs -> m Deity,
    character :: [Character]
  } deriving (Generic, GQLType)

data DeityArgs = DeityArgs
  { name      :: Text -- Required Argument
  , bornPlace :: Maybe City -- Optional Argument
  } deriving (Generic)

resolveDeity :: DeityArgs -> IORes e Deity
resolveDeity DeityArgs { name, bornPlace } = liftEither $ dbDeity name Nothing

resolveCharacter :: [Character]
resolveCharacter =
  [ CharacterHuman H.Human { H.name = "Odysseus", H.bornAt = Ithaca }
  , CharacterDeity Deity { fullName = "Hades"
                         , power    = Nothing
                         , realm    = Underworld
                         }
  , Creature { creatureName = "Lamia", creatureAge = 205 }
  , BoxedDeity
    { boxedDeity = Deity { fullName = "Hades"
                         , power    = Nothing
                         , realm    = Underworld
                         }
    }
  , SomeScalarRecord { scalarText = "Some Text" }
  ---
  , SomeDeity
    (Deity { fullName = "Hades", power = Nothing, realm = Underworld })
  , SomeScalar 12
  , SomeMutli 21 "some text"
  , Zeus
  , Cronus
  ]

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver = GQLRootResolver
  { queryResolver = Query { deity = resolveDeity, character = resolveCharacter }
  , mutationResolver = Undefined
  , subscriptionResolver = Undefined
  }

mythologyApi :: B.ByteString -> IO B.ByteString
mythologyApi = interpreter rootResolver
