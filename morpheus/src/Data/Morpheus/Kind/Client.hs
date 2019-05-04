{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Data.Morpheus.Kind.Client where

--import           Data.Morpheus.Kind.Internal (GQL, GQLObject, OBJECT, SCALAR, introspection)
import           Data.Text (Text)

-- import qualified Data.Text                   as T (concat)
-- Client Side
-- define GQL Schema
--newtype User =
--  User Text
--  deriving (Show, Eq, GQLObject)
--
-- newtype Odd =
--  Odd Int
--  deriving (Show, Read, GQLScalar)
-- type instance GQL Odd = SCALAR
-- type instance GQL User = OBJECT
-- Resolvers
resolveTypes :: Text
resolveTypes = ""
-- T.concat [introspection (Odd 3), " and ", introspection (User "David")]
