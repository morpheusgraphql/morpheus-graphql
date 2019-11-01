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
import           Data.Text                  (Text)

defineByIntrospectionFile
  "./assets/introspection.json"
  [gql|
   
    # Query Hero with Compile time Validation
    query GetUser ($coordinates: Coordinates!)
      {
        myUser: user {
           boo3: name
           myUserEmail: email
           address (coordinates: $coordinates ){
             city
           }
           customAdress: address (coordinates: $coordinates ){
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
    query GetHero ($god: Realm, $id: String!)
      {
        deity (mythology:$god) {
          power
          fullName
        }
        character(characterID: $id ) {
          ...on Creature {
            name
            immortality
          }
          ...on Human {
            lifetime
            profession
          }
        }
        char2: character(characterID: $id ) {
          ...on Creature {
              cName: name
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
    GetHeroArgs
      { getHeroArgsGod =
          Just Realm {realmOwner = "Zeus", realmAge = Just 10, realmRealm = Nothing, realmProfession = Just Artist}
      , getHeroArgsId = "Hercules"
      }

fetUser :: (ByteString -> IO ByteString) -> IO (Either String GetUser)
fetUser api = fetch api userArgs
  where
    userArgs :: Args GetUser
    userArgs =
      GetUserArgs {getUserArgsCoordinates = Coordinates {coordinatesLongitude = [], coordinatesLatitude = String "1"}}
