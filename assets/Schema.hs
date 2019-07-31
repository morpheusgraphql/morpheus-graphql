{-# LANGUAGE DeriveGeneric #-}

module Schema where

import  GHC.Generics  (Generic)
import  Data.Morpheus.KIND  (SCALAR,ENUM,INPUT_OBJECT,OBJECT,OBJECT)
import  Data.Morpheus.Types  (ResM,GQLType(..))



 ---- GQL Query ------------------------------- 
data Query = Query 
  { deity :: ArgDeity -> ResM Deity
  ,  character :: ArgCharacter -> ResM Character
  } deriving (Generic)

data ArgDeity = ArgDeity 
  { name :: Maybe [Maybe [Maybe [[Maybe [String]]]]]
  ,  mythology :: Maybe String
  } deriving (Generic)

data ArgCharacter = ArgCharacter 
  { characterID :: String
  ,  age :: Maybe Int
  } deriving (Generic)

instance GQLType Query where
  KIND Query = OBJECT



 ---- GQL Mutation ------------------------------- 
data Mutation = Mutation 
  { createDeity :: ArgCreateDeity -> ResM Deity
  ,  createCharacter :: ArgCreateCharacter -> ResM Character
  } deriving (Generic)

data ArgCreateDeity = ArgCreateDeity 
  { deityName :: Maybe [Maybe [Maybe [[Maybe [String]]]]]
  ,  deityMythology :: Maybe String
  } deriving (Generic)

data ArgCreateCharacter = ArgCreateCharacter 
  { charRealm :: Realm
  ,  charMutID :: String
  } deriving (Generic)

instance GQLType Mutation where
  KIND Mutation = OBJECT



 ---- GQL City ------------------------------- 
data City = 
  Athens
  | Ithaca
  | Sparta
  | Troy deriving (Generic)

instance GQLType City where
  KIND City = ENUM



 ---- GQL Power ------------------------------- 
data Power = Power Int String

instance GQLType Power where
  KIND Power = SCALAR



 ---- GQL Realm ------------------------------- 
data Realm = Realm 
  { owner :: String
  ,  place :: Maybe Int
  } deriving (Generic)

instance GQLType Realm where
  KIND Realm = INPUT_OBJECT



 ---- GQL Deity ------------------------------- 
data Deity = Deity 
  { fullName :: () -> ResM String
  ,  power :: () -> ResM (Maybe String)
  } deriving (Generic)

instance GQLType Deity where
  KIND Deity = OBJECT



 ---- GQL Creature ------------------------------- 
data Creature = Creature 
  { creatureName :: () -> ResM String
  ,  abilities :: () -> ResM (Maybe String)
  } deriving (Generic)

instance GQLType Creature where
  KIND Creature = OBJECT



 ---- GQL Human ------------------------------- 
data Human = Human 
  { humanName :: () -> ResM String
  ,  profession :: () -> ResM (Maybe String)
  } deriving (Generic)

instance GQLType Human where
  KIND Human = OBJECT



 ---- GQL Character ------------------------------- 
data Character = 
  Character_CREATURE Creature
  | Character_DEITY Deity
  | Character_HUMAN Human deriving (Generic)

instance GQLType Character where
  KIND Character = UNION