{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Server.Mythology.API
  ( mythologyApi
  )
where

import qualified Data.ByteString.Lazy.Char8    as B
import           Data.Morpheus                  ( interpreter )
import           Data.Morpheus.Types            ( GQLRootResolver(..)
                                                , GQLType
                                                , IORes
                                                , Undefined(..)
                                                , liftEither
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Server.Mythology.Character.Deity
                                                ( Deity(..)
                                                , dbDeity
                                                )

newtype Query m = Query
  { deity :: DeityArgs -> m Deity
  } deriving (Generic, GQLType)

data DeityArgs = DeityArgs
  { name      :: Text -- Required Argument
  , mythology :: Maybe Text -- Optional Argument
  } deriving (Generic)

resolveDeity :: DeityArgs -> IORes e Deity
resolveDeity DeityArgs { name, mythology } =
  liftEither $ dbDeity name mythology

rootResolver :: GQLRootResolver IO () Query Undefined Undefined
rootResolver = GQLRootResolver { queryResolver = Query { deity = resolveDeity }
                               , mutationResolver = Undefined
                               , subscriptionResolver = Undefined
                               }

mythologyApi :: B.ByteString -> IO B.ByteString
mythologyApi = interpreter rootResolver
