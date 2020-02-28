{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Server.Mythology.Character
  ( Deity(..)
  , dbDeity
  , Human(..)
  , someHuman
  , someDeity
  )
where

import           Data.Morpheus.Types            ( GQLType(..) )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Server.Mythology.Place         ( Realm(..)
                                                , City(..)
                                                )

data Deity = Deity
  { name :: Text -- Non-Nullable Field
  , power    :: Maybe Text -- Nullable Field
  , realm    :: Realm
  , bornAt   :: Maybe City
  } deriving (Generic,GQLType)


data Human m = Human 
  { name :: m Text
  , bornAt :: m City
  } deriving (Generic,GQLType)


someHuman :: Applicative m => Human m 
someHuman = Human { name = pure "Odysseus", bornAt = pure Ithaca }

someDeity :: Deity
someDeity = Deity { name   = "Morpheus"
                  , power  = Just "Shapeshifting"
                  , realm  = Dream
                  , bornAt = Nothing
                  }

dbDeity :: Text -> Maybe City -> IO (Either String Deity)
dbDeity _ bornAt = return $ Right $ Deity { name   = "Morpheus"
                                          , power  = Just "Shapeshifting"
                                          , realm  = Dream
                                          , bornAt
                                          }
