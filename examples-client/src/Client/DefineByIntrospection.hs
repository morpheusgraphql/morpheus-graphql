{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Client.DefineByIntrospection
  ( fetchUsers,
  )
where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Morpheus.Client
  ( Fetch (..),
    defineByDocumentFile,
    defineByIntrospectionFile,
    gql,
  )
import Data.Morpheus.Types.Internal.AST (ScalarValue (..))
import Data.Text (Text)

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

usersApi :: ByteString -> IO ByteString
usersApi req = do
  print req
  putStrLn ""
  pure $
    "{\"data\":{"
      <> "\"myUser\":{ "
      <> "   \"boo3\": \"name\","
      <> "   \"myUserEmail\": \"some field\","
      <> "   \"address\":{ \"city\":\"some city\" },"
      <> "   \"customAdress\":{ \"customCity\":\"some custom city\" }"
      <> "},"
      <> " \"user\":{ \"email\":\"some email\" }"
      <> "}}"

fetchUsers :: IO (Either String GetUser)
fetchUsers = fetch usersApi userArgs
  where
    userArgs :: Args GetUser
    userArgs =
      GetUserArgs
        { getUserArgsCoordinates =
            Coordinates
              { coordinatesLongitude = [],
                coordinatesLatitude = String "1"
              }
        }
