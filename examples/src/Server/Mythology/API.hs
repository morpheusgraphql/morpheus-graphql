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
    Creature {
      creatureName :: Text,
      creatureAge :: Int
    }
  | Zeus
  | Human' H.Human -- Only Human' should generate direct link
  | Deity' Deity -- Only Deity' should generate direct link
  | SomeScalar Int
  | SomeScalarRecord { scalarText :: Text }
  | SomeMutli Int Text
  | BoxedDeity { boxedDeity :: Deity}
  | Cronus deriving (Generic, GQLType)

data Query m = Query
  { deity :: DeityArgs -> m Deity,
    character :: [Character]
  } deriving (Generic, GQLType)

data DeityArgs = DeityArgs
  { name      :: Text -- Required Argument
  , mythology :: Maybe Text -- Optional Argument
  } deriving (Generic)

resolveDeity :: DeityArgs -> IORes e Deity
resolveDeity DeityArgs { name, mythology } =
  liftEither $ dbDeity name mythology

resolveCharacter :: [Character]
resolveCharacter =
  [ Human' H.Human { H.name = "Odysseus", H.bornAt = Ithaca }
  , Deity' Deity { fullName = "Hades", power = Nothing, realm = Underworld }
  , Cronus
  , Zeus
  , Creature { creatureName = "Lamia", creatureAge = 205 }
  , SomeScalar 12
  ]

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver = GQLRootResolver
  { queryResolver = Query { deity = resolveDeity, character = resolveCharacter }
  , mutationResolver = Undefined
  , subscriptionResolver = Undefined
  }

mythologyApi :: B.ByteString -> IO B.ByteString
mythologyApi = interpreter rootResolver
