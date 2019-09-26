{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Client.Client where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Morpheus.Client       (Fetch (..), defineByDocumentFile, defineByIntrospectionFile, gql)
import           Data.Morpheus.Types            (ScalarValue (..))

defineByIntrospectionFile
  "./assets/introspection.json"
  [gql|
    # Query Hero with Compile time Validation
    query GetUser ($userCoordinates: Coordinates!)
      {
        myUser: user {
           boo3: name
           email
           address (coordinates: $userCoordinates ){
            city
           }
        }
      }
  |]

defineByDocumentFile
  "./assets/simple.gql"
  [gql|
    # Query Hero with Compile time Validation
    query GetHero ($god: Realm, $charID: String!)
      {
        deity (mythology:$god) {
          power
          fullName
        }
        character(characterID: $charID ) {
          ...on Creature {
            creatureName
          }
          ...on Human {
            lifetime
            profession
          }
        }
      }
  |]

ioRes :: ByteString -> IO ByteString
ioRes req = do
  print req
  return
    "{\"data\":{\"deity\":{ \"fullName\": \"name\" }, \"character\":{ \"__typename\":\"Human\", \"lifetime\": \"Lifetime\", \"profession\": \"Artist\" }  }}"

fetchHero :: IO (Either String GetHero)
fetchHero = fetch ioRes GetHeroArgs {god = Just Realm {owner = "Zeus", surface = Just 10}, charID = "Hercules"}

