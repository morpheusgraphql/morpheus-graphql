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

resolvers :: (Monad m) => RootResolverValue e m
resolvers =
  queryResolvers
    [ ( "Query",
        traverse
          ( const
              $ object
                [ ("success", pure "success"),
                  ("forbidden", pure "forbidden!"),
                  ("limited", pure "num <= 5")
                ]
          )
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
max5 _ query =
  case getChild ("limited" :: Text) query >>= getArgument ("num" :: Text) of
    Just (Number n) | n > 5 -> Left ("num " <> show n <> " is greater then 5! but found ")
    _ -> pure ()

runAPIConstraints :: FileUrl -> FileUrl -> TestTree
runAPIConstraints url = testApi api
  where
    api :: GQLRequest -> IO GQLResponse
    api req = do
      app <- withConstraint max5 . withConstraint forbidden <$> getApp url
      runApp app req
