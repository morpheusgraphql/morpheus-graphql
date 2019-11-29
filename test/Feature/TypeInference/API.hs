{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Feature.TypeInference.API
  ( api
  )
where

import           Data.Morpheus                  ( interpreter )
import           Data.Morpheus.Types            ( GQLRequest
                                                , GQLResponse
                                                , GQLRootResolver(..)
                                                , GQLType(..)
                                                , Undefined(..)
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

data Power = Thunderbolts | Shapeshift | Hurricanes
  deriving (Generic,GQLType)

data Deity = Deity {
  name :: Text ,
  power :: Power
} deriving(Generic,GQLType)

deityRes :: Deity
deityRes = Deity { name = "Morpheus", power = Shapeshift }


data Character  =
  CharacterDeity Deity -- Only <tycon name><type ref name> should generate direct link
  -- RECORDS
  | Creature { creatureName :: Text, creatureAge :: Int }
  | BoxedDeity { boxedDeity :: Deity}
  | ScalarRecord { scalarText :: Text }
  --- Types 
  | CharacterInt Int -- all scalars mus be boxed
  -- Types
  | SomeDeity Deity
  | SomeMutli Int Text
  --- ENUMS
  | Zeus
  | Cronus deriving (Generic, GQLType)

data Query (m :: * -> *) = Query
  { deity :: Deity,
    character :: [Character]
  } deriving (Generic, GQLType)

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver = GQLRootResolver
  { queryResolver        = Query { deity = deityRes, character }
  , mutationResolver     = Undefined
  , subscriptionResolver = Undefined
  }
 where
  character :: [Character]
  character =
    [ CharacterDeity deityRes
    , Creature { creatureName = "Lamia", creatureAge = 205 }
    , BoxedDeity { boxedDeity = deityRes }
    , ScalarRecord { scalarText = "Some Text" }
      ---
    , SomeDeity deityRes
    , CharacterInt 12
    , SomeMutli 21 "some text"
    , Zeus
    , Cronus
    ]


api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
