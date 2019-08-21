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

import           Control.Monad          ((>=>))
import           Data.Aeson             (FromJSON, ToJSON (..), eitherDecode, encode)
import           Data.ByteString.Lazy   (ByteString)
import           Data.Text              (pack)
import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Types.IO (GQLRequest (..), JSONResponse (..))

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
      gqlReq = GQLRequest {operationName = Just (pack opName), query = pack strQuery, variables = Just (toJSON vars)}
      ----
      processResponse JSONResponse {responseData = Just x} = pure x
      processResponse invalidResponse                      = fail $ show invalidResponse
  fetch :: (Monad m, FromJSON a) => (ByteString -> m ByteString) -> Args a -> m (Either String a)

deriveFetch :: Type -> String -> String -> Q [Dec]
deriveFetch argDatatype typeName query =
  pure <$> instanceD (cxt []) (appT (conT ''Fetch) (conT $ mkName typeName)) methods
  where
    methods =
      [ funD (mkName "fetch") [clause [] (normalB [|__fetch query typeName|]) []]
      , pure $ TySynInstD ''Args (TySynEqn [ConT $ mkName typeName] argDatatype)
      ]
