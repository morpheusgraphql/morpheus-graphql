{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module APIConstraints
  ( runAPIConstraints,
  )
where

import Data.Aeson (Value (..))
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Morpheus.App
  ( APIConstraint,
    App,
    mkApp,
    runApp,
    withConstraint,
  )
import Data.Morpheus.App.Internal.Resolving (resultOr)
import Data.Morpheus.App.NamedResolvers
  ( RootResolverValue,
    object,
    queryResolvers,
  )
import Data.Morpheus.Core
  ( parseSchema,
  )
import Data.Morpheus.Types.IO
  ( GQLRequest,
    GQLResponse,
  )
import Data.Morpheus.Types.Internal.AST
  ( Schema,
    VALID,
  )
import Data.Morpheus.Types.SelectionTree
  ( SelectionTree (..),
  )
import Relude hiding (ByteString)
import Test.Morpheus
  ( FileUrl,
    testApi,
  )
import Test.Tasty
  ( TestTree,
  )

resolvers :: Monad m => RootResolverValue e m
resolvers =
  queryResolvers
    [ ( "Query",
        const $
          object
            [ ("success", pure "success"),
              ("forbidden", pure "forbidden!"),
              ("limited", pure "is less then 5")
            ]
      )
    ]

getSchema :: String -> IO (Schema VALID)
getSchema url = LBS.readFile url >>= resultOr (fail . show) pure . parseSchema

getApp :: FileUrl -> IO (App e IO)
getApp _ = (`mkApp` resolvers) <$> getSchema "test/api-constraints/schema.gql"

forbidden :: APIConstraint
forbidden _ query
  | hasChild ("forbidden" :: String) query = Left "no forbidden field!"
  | otherwise = Right ()

max5 :: APIConstraint
max5 _ query = do
  let value = getChild ("limited" :: String) query >>= getArgument ("num" :: String)
  case value of
    Just (Number num) | num >= 5 -> Left ("no num should be less then 5! but found " <> show num)
    _ -> pure ()

runAPIConstraints :: FileUrl -> FileUrl -> TestTree
runAPIConstraints url = testApi api
  where
    api :: GQLRequest -> IO GQLResponse
    api req = do
      app <- withConstraint max5 . withConstraint forbidden <$> getApp url
      runApp app req
