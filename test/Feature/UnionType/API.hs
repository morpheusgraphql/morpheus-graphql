{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Feature.UnionType.API
  ( api
  )
where

import           Data.Morpheus                  ( interpreter )
import           Data.Morpheus.Kind             ( OBJECT
                                                , UNION
                                                )
import           Data.Morpheus.Types            ( GQLRequest
                                                , GQLResponse
                                                , GQLRootResolver(..)
                                                , GQLType(..)
                                                , IORes
                                                , Undefined(..)
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )

instance GQLType A where
  type KIND A = OBJECT

instance GQLType B where
  type KIND B = OBJECT

instance GQLType C where
  type KIND C = OBJECT

instance GQLType Sum where
  type KIND Sum = UNION

data A = A
  { aText :: Text
  , aInt  :: Int
  } deriving (Generic)

data B = B
  { bText :: Text
  , bInt  :: Int
  } deriving (Generic)

data C = C
  { cText :: Text
  , cInt  :: Int
  } deriving (Generic)

data Sum
  = SumA A
  | SumB B
  deriving (Generic)

data Query m = Query
  { union :: () -> m [Sum]
  , fc    :: C
  } deriving (Generic, GQLType)

resolveUnion :: () -> IORes () [Sum]
resolveUnion _ =
  return [SumA A { aText = "at", aInt = 1 }, SumB B { bText = "bt", bInt = 2 }]

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver = GQLRootResolver
  { queryResolver        = Query { union = resolveUnion
                                 , fc    = C { cText = "", cInt = 3 }
                                 }
  , mutationResolver     = Undefined
  , subscriptionResolver = Undefined
  }

api :: GQLRequest -> IO GQLResponse
api = interpreter rootResolver
