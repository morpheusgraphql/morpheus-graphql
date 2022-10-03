{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module APIConstraints
  ( runAPIConstraints,
  )
where

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
  ( GQLRequest (..),
    GQLResponse,
  )
import Data.Morpheus.Types.Internal.AST
  ( Schema,
    VALID,
  )
import Data.Morpheus.Types.SelectionTree
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
              ("forbidden", pure "forbidden!")
            ]
      )
    ]

getSchema :: String -> IO (Schema VALID)
getSchema url = LBS.readFile url >>= resultOr (fail . show) pure . parseSchema

getApp :: FileUrl -> IO (App e IO)
getApp _ = (`mkApp` resolvers) <$> getSchema "test/api-constraints/schema.gql"

constraint :: APIConstraint
constraint _ operation = do
  let forbidden = lookupChild "forbidden" (operationSelectionTree operation)
  maybe (Right ()) (const (Left "no forbidden field!")) forbidden

runAPIConstraints :: FileUrl -> FileUrl -> TestTree
runAPIConstraints url = testApi api
  where
    api :: GQLRequest -> IO GQLResponse
    api req = do
      app <- withConstraint constraint <$> getApp url
      runApp app req
