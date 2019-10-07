{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Client.Client
  ( fetchHero
  , fetUser
  ) where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Morpheus.Client       (Fetch (..), defineByDocumentFile, defineByIntrospectionFile, gql)
import           Data.Morpheus.Types        (ScalarValue (..))

defineByIntrospectionFile
  "./assets/introspection.json"
  [gql|
    # Query Hero with Compile time Validation
    query GetUser ($userCoordinates: Coordinates!)
      {
        myUser: user {
           boo3: name
           myUserEmail: email
           address (coordinates: $userCoordinates ){
             city
           }
           customAdress: address (coordinates: $userCoordinates ){
               customCity: city
           }
        }
        user {
          email
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
        char2: character(characterID: $charID ) {
          ...on Creature {
              cName: creatureName
          }
          ...on Human {
              lTime: lifetime
              prof: profession
          }
        }
      }
  |]

ioRes :: ByteString -> IO ByteString
ioRes req = do
  print req
  return
    "{\"data\":{\"deity\":{ \"fullName\": \"name\" }, \"character\":{ \"__typename\":\"Human\", \"lifetime\": \"Lifetime\", \"profession\": \"Artist\" } ,  \"char2\":{ \"__typename\":\"Human\", \"lTime\": \"time\", \"prof\": \"Artist\" }  }}"

fetchHero :: IO (Either String GetHero)
fetchHero =
  fetch
    ioRes
    GetHeroArgs {god = Just Realm {owner = "Zeus", surface = Just 10, realmRec = Nothing}, charID = "Hercules"}

fetUser :: (ByteString -> IO ByteString) -> IO (Either String GetUser)
fetUser api = fetch api userArgs
  where
    userArgs :: Args GetUser
    userArgs = GetUserArgs {userCoordinates = Coordinates {longitude = [], latitude = String "1"}}
