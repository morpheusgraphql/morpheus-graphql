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
import Client.NewUsers
  ( subscribeNewUsers,
  )
import Client.Users
  ( fetchUser,
  )

main :: IO ()
main = do
  fetchUser >>= print
  putStrLn "\n"

  fetchHero >>= print
  putStrLn "\n"

  fetchUsers >>= print
  putStrLn "\n"

  subscribeNewUsers
