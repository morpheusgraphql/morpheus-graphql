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
import Client.StarWarsClient
  ( fetchFilms,
  )
import Client.Users
  ( fetchUser,
  )

main :: IO ()
main = do
  -- fetchUser (runApp app) >>= print
  fetchFilms >>= print
  putStrLn "\n"
  fetchUsers >>= print
  putStrLn "\n"
  fetchHero >>= print
