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

main :: IO ()
main = do
  fetchFilms >>= print
  putStrLn "\n"
  fetchUsers >>= print
  putStrLn "\n"
  fetchHero >>= print
