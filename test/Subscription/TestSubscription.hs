{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts   #-}

module Subscription.TestSubscription
  ( testSubscription
  ) where

-- import qualified Data.Text.Lazy             as LT (toStrict)
-- import           Data.Text.Lazy.Encoding    (decodeUtf8)

import           Control.Monad.IO.Unlift        ( MonadUnliftIO
                                                , withRunInIO
                                                )
import           Data.Aeson                     ( FromJSON
                                                , Value
                                                , decode
                                                , encode
                                                )
import           Data.ByteString.Lazy.Char8     ( ByteString )
import qualified Data.ByteString.Lazy.Char8     as LB 
                                                (unpack)
import           Data.Morpheus.Types            ( GQLRequest (..)
                                                , GQLResponse (..)
                                                , Input
                                                , Stream
                                                )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text, unpack)
import qualified Data.Text                      as T 
                                                (concat)
import           Control.Monad.IO.Class         ( MonadIO(..) )
-- import           GHC.Generics
import           Lib                            ( getCases
                                                , getGQLBody
                                                , getResponseBody
                                                , maybeVariables
                                                )
import           Test.Tasty.HUnit               ( assertFailure
                                                , testCase
                                                )
import           Test.Tasty                     ( TestTree
                                                , testGroup
                                                )
import           Data.Morpheus.Server           ( subscriptionApp
                                                , ServerConstraint
                                                )
import           Data.Morpheus.Types.Internal.Subscription           
                                                ( WS
                                                , Scope(..)
                                                , Store(..)
                                                , runStreamWS
                                                , connect
                                                , GQLChannel(..)
                                                )
import           Types                          ( Case(..)
                                                , Name
                                                , testWith
                                                )           
import          Control.Monad.State.Lazy        ( StateT
                                                , get 
                                                , put
                                                , runStateT
                                                )



-- packGQLRequest :: ByteString -> Maybe Value -> GQLRequest
-- packGQLRequest queryBS variables = GQLRequest 
--   { operationName = Nothing
--   , query = LT.toStrict $ decodeUtf8 queryBS
--   , variables
--   }

-- data Case = Case
--   { path        :: Text
--   , description :: String
--   } deriving (Generic, FromJSON)

-- testSubscription :: (GQLRequest -> IO GQLResponse) -> Text -> IO TestTree
-- testSubscription api dir = do
--   cases' <- getCases (unpack dir)
--   test' <- sequence $ testByFiles api <$> map (\x -> x {path = T.concat [dir, "/", path x]}) cases'
--   return $ testGroup (unpack dir) test'

-- testByFiles :: (GQLRequest -> IO GQLResponse) -> Case -> IO TestTree
-- testByFiles testApi Case {path, description} = do
--   testCaseQuery <- getGQLBody path
--   testCaseVariables <- maybeVariables path
--   expectedResponse <- getResponseBody path
--   actualResponse <- encode <$> testApi (packGQLRequest testCaseQuery testCaseVariables)
--   case decode actualResponse of
--     Nothing -> assertFailure "Bad Response"
--     Just response -> return $ testCase (unpack path ++ " | " ++ description) $ customTest expectedResponse response
--       where customTest expected value
--               | expected == value = return ()
--               | otherwise =
--                 assertFailure $ LB.unpack $ "expected: \n " <> encode expected <> " \n but got: \n " <> actualResponse



type SubM = StateT ByteString IO

mockWSApp ::
  (Input WS -> Stream WS e SubM)
  -> Input WS
  -> SubM ()
mockWSApp api input
  = runStreamWS
      ScopeWS 
      { update = undefined 
      ,  listener = pure "{ \"type\":\"bla\" }"
      , callback = put
      }
      (api input)
  
testSubscription 
  ::  
    (Eq (StreamChannel e)
    , GQLChannel e
    )
  => ( Input WS -> Stream WS e SubM ) -> Name -> IO TestTree
testSubscription api = testWith (testSubCase api)

testSubCase 
  :: (Eq (StreamChannel e)
    , GQLChannel e
    )
  => ( Input WS -> Stream WS e SubM ) -> Case -> IO TestTree
testSubCase api Case {path, description} = do
  -- testCaseQuery <- getGQLBody path
  -- testCaseVariables <- maybeVariables path
  -- expectedResponse <- getResponseBody path
  -- actualResponse <- encode <$> testApi (packGQLRequest testCaseQuery testCaseVariables)
  -- case decode actualResponse of
  --   Nothing -> assertFailure "Bad Response"
  --   Just response -> return $ testCase (unpack path ++ " | " ++ description) $ customTest expectedResponse response

  input <- connect 
  (_,result) <-  runStateT (mockWSApp api input) "bla"
  pure 
    $ testCase "test subscription"
    $ customTest "some text"  result

customTest :: ByteString -> ByteString -> IO () 
customTest expected value
      | expected == value = return ()
      | otherwise =
        assertFailure $ LB.unpack $ "expected: \n " <> expected <> " \n but got: \n " <> value

