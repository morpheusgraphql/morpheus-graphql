{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE QuasiQuotes             #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE TypeFamilies            #-}

module Data.Morpheus.Execution.Client.Fetch
  ( Fetch(..)
  , deriveFetch
  ) where

import           Control.Monad                   ((>=>))
import           Data.Aeson                      (FromJSON, ToJSON (..), eitherDecode, encode)
import           Data.ByteString.Lazy            (ByteString)
import           Data.Text                       (pack)
import           Language.Haskell.TH

import qualified Data.Aeson       as A
import qualified Data.Aeson.Types as A

--
-- MORPHEUS
import           Data.Morpheus.Types.Internal.TH (instanceHeadT)
import           Data.Morpheus.Types.IO          (GQLRequest (..), JSONResponse (..))

fixVars :: A.Value -> Maybe A.Value
fixVars x
  | x == A.emptyArray = Nothing
  | otherwise         = Just x

class Fetch a where
  type Args a :: *
  __fetch ::
       (Monad m, Show a, ToJSON (Args a), FromJSON a)
    => String
    -> String
    -> (ByteString -> m ByteString)
    -> Args a
    -> m (Either String a)
  __fetch strQuery opName trans vars = (eitherDecode >=> processResponse) <$> trans (encode gqlReq)
    where
      gqlReq = GQLRequest {operationName = Just (pack opName), query = pack strQuery, variables = fixVars (toJSON vars)}
      -------------------------------------------------------------
      processResponse JSONResponse {responseData = Just x} = pure x
      processResponse invalidResponse                      = fail $ show invalidResponse
  fetch :: (Monad m, FromJSON a) => (ByteString -> m ByteString) -> Args a -> m (Either String a)

deriveFetch :: Type -> String -> String -> Q [Dec]
deriveFetch argDatatype typeName query = pure <$> instanceD (cxt []) iHead methods
  where
    iHead = instanceHeadT ''Fetch typeName []
    methods =
      [ funD 'fetch [clause [] (normalB [|__fetch query typeName|]) []]
      , pure $ TySynInstD ''Args (TySynEqn [ConT $ mkName typeName] argDatatype)
      ]
