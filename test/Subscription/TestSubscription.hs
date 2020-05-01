{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Subscription.TestSubscription
  ( testSubscription,
    SubM,
  )
where

-- import qualified Data.Text.Lazy             as LT (toStrict)
-- import           Data.Text.Lazy.Encoding    (decodeUtf8)
import Control.Monad.State.Lazy
  ( StateT,
    runStateT,
    state,
  )
import Data.ByteString.Lazy.Char8 (ByteString)
-- import qualified Data.ByteString.Lazy.Char8 as LB
--   ( unpack,
--   )
import Data.Morpheus.Types
  ( Input,
    Stream,
  )
import Data.Morpheus.Types.Internal.Subscription
  ( ClientConnectionStore,
    GQLChannel (..),
    Scope (..),
    WS,
    connect,
    empty,
    runStreamWS,
  )
import Data.Semigroup ((<>))
import Test.Tasty
  ( TestTree,
    testGroup,
  )
import Test.Tasty.HUnit
  ( assertFailure,
    testCase,
  )
import Types
  ( Case (..),
    Name,
    testWith,
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

data Session e = Session
  { inputs :: [ByteString],
    outputs :: [ByteString],
    store :: ClientConnectionStore e (SubM e)
  }

type SubM e = StateT (Session e) IO

mockWSApp ::
  (Input WS -> Stream WS e (SubM e)) ->
  Input WS ->
  SubM e ()
mockWSApp api input =
  runStreamWS
    ScopeWS
      { update = state . updateStore,
        -- use quick check for responce type
        listener = state readInput,
        callback = state . addOutput
      }
    (api input)

addOutput :: ByteString -> Session e -> ((), Session e)
addOutput x (Session i xs st) = ((), Session i (xs <> [x]) st)

updateStore :: (ClientConnectionStore e (SubM e) -> ClientConnectionStore e (SubM e)) -> Session e -> ((), Session e)
updateStore up (Session i o st) = ((), Session i o (up st))

readInput :: Session e -> (ByteString, Session e)
readInput (Session (i : inputs) o s) = (i, Session inputs o s)
readInput (Session [] o s) = ("<Error>", Session [] o s)

testSubscription ::
  ( Eq (StreamChannel e),
    GQLChannel e
  ) =>
  (Input WS -> Stream WS e (SubM e)) ->
  Name ->
  IO TestTree
testSubscription api = testWith (testSubCase api)

startCase ::
  (Input WS -> Stream WS e (SubM e)) ->
  Input WS ->
  [ByteString] ->
  IO (Session e)
startCase api input inputs = snd <$> runStateT (mockWSApp api input) (Session inputs [] empty)

testSubCase ::
  ( Eq (StreamChannel e),
    GQLChannel e
  ) =>
  (Input WS -> Stream WS e (SubM e)) ->
  Case ->
  IO TestTree
testSubCase api Case {path, description} = do
  input <- connect
  Session {inputs, outputs, store} <- startCase api input ["{ \"type\":\"bla\" }"]
  pure
    $ testCase "fail on unknown request type"
    $ expectedStream
      ["Unknown Request type \"bla\"."]
      outputs

expectedStream :: [ByteString] -> [ByteString] -> IO ()
expectedStream expected value
  | expected == value = return ()
  | otherwise =
    assertFailure $ "expected: \n " <> show expected <> " \n but got: \n " <> show value
