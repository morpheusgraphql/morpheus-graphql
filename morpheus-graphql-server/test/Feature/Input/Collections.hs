{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Feature.Input.Collections
  ( api,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Morpheus.Server (deriveApp, runApp)
import Data.Morpheus.Server.Types
  ( App,
    Arg (..),
    GQLRequest,
    GQLResponse,
    GQLType (..),
    InputTypeNamespace (..),
    RootResolver (..),
    Undefined,
    defaultRootResolver,
    typeDirective,
  )
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text
import Data.Vector (Vector)
import GHC.Generics (Generic)

-- query
testRes :: (Applicative m) => Arg "value" a -> m a
testRes = pure . argValue

type Coll m a = Arg "value" a -> m a

data Product = Product Text Int Bool (Maybe Double)
  deriving (Generic)

instance GQLType Product where
  directives _ = typeDirective (InputTypeNamespace "Input")

-- resolver
data Query m = Query
  { testSet :: Coll m (Set Int),
    testNonEmpty :: Coll m (NonEmpty Int),
    tesSeq :: Coll m (Seq Int),
    testVector :: Coll m (Vector Int),
    testProduct :: Coll m Product,
    testTuple :: Coll m (Text, Int),
    testMap :: Coll m (Map Text Int),
    testAssoc :: Coll m [(Text, Int)]
  }
  deriving (Generic, GQLType)

rootResolver :: RootResolver IO () Query Undefined Undefined
rootResolver =
  defaultRootResolver
    { queryResolver =
        Query
          { testSet = testRes,
            testNonEmpty = testRes,
            tesSeq = testRes,
            testVector = testRes,
            testTuple = testRes,
            testProduct = testRes,
            testMap = testRes,
            testAssoc = testRes
          }
    }

app :: App () IO
app = deriveApp rootResolver

api :: GQLRequest -> IO GQLResponse
api = runApp app
