{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Client.DefineByIntrospection
  ( fetchUsers,
  )
import Client.Mythology
  ( fetchHero,
  )
import Client.NewUsers (newUsers)
import Client.StarWarsClient (fetchFilms)
import Client.Users
  ( fetchUser,
  )
import Data.Morpheus.Client
  ( GQLClient,
    forEach,
    single,
    withHeaders,
  )

userClient :: GQLClient
userClient = "http://localhost:3000" `withHeaders` [("custom-header", "custom-value")]

userWSClient :: GQLClient
userWSClient = "ws://localhost:3000"

mythologyClient :: GQLClient
mythologyClient = "http://localhost:3000/mythology"

starWarsClient :: GQLClient
starWarsClient = "https://swapi.graph.cool"

main :: IO ()
main = do
  fetchUser userClient >>= single >>= print
  putStrLn "\n"

  fetchHero mythologyClient >>= single >>= print
  putStrLn "\n"

  fetchUsers >>= print
  putStrLn "\n"

  newUsers userWSClient >>= forEach print

  fetchFilms starWarsClient >>= single >>= print
