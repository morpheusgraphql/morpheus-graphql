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
import Client.Users
  ( fetchUser,
  )
import Data.Morpheus.Client (forEach, single)

main :: IO ()
main = do
  fetchUser >>= single >>= print
  putStrLn "\n"

  fetchHero >>= single >>= print
  putStrLn "\n"

  fetchUsers >>= print
  putStrLn "\n"

  newUsers >>= forEach print
