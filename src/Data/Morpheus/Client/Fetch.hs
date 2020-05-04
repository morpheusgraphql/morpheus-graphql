{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Client.Fetch
  ( Fetch (..),
    deriveFetch,
  )
where

import Control.Monad ((>=>))
import Data.Aeson
  ( FromJSON,
    ToJSON (..),
    eitherDecode,
    encode,
  )
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.ByteString.Lazy (ByteString)
--
-- MORPHEUS

import Data.Morpheus.Types.IO
  ( GQLRequest (..),
    JSONResponse (..),
  )
import Data.Morpheus.Types.Internal.TH
  ( instanceHeadT,
    typeInstanceDec,
  )
import Data.Text
  ( Text,
    pack,
    unpack,
  )
import Language.Haskell.TH

fixVars :: A.Value -> Maybe A.Value
fixVars x
  | x == A.emptyArray = Nothing
  | otherwise = Just x

class Fetch a where
  type Args a :: *
  __fetch ::
    (Monad m, Show a, ToJSON (Args a), FromJSON a) =>
    String ->
    Text ->
    (ByteString -> m ByteString) ->
    Args a ->
    m (Either String a)
  __fetch strQuery opName trans vars = (eitherDecode >=> processResponse) <$> trans (encode gqlReq)
    where
      gqlReq = GQLRequest {operationName = Just opName, query = pack strQuery, variables = fixVars (toJSON vars)}
      -------------------------------------------------------------
      processResponse JSONResponse {responseData = Just x} = Right x
      processResponse invalidResponse = Left (show invalidResponse)
  fetch :: (Monad m, FromJSON a) => (ByteString -> m ByteString) -> Args a -> m (Either String a)

deriveFetch :: Type -> Text -> String -> Q [Dec]
deriveFetch resultType typeName queryString =
  pure <$> instanceD (cxt []) iHead methods
  where
    iHead = instanceHeadT ''Fetch typeName []
    methods =
      [ funD 'fetch [clause [] (normalB [|__fetch queryString typeName|]) []],
        pure $ typeInstanceDec ''Args (ConT $ mkName $ unpack typeName) resultType
      ]
