{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE QuasiQuotes             #-}
{-# LANGUAGE TemplateHaskell         #-}
{-# LANGUAGE TypeFamilies            #-}

module Data.Morpheus.Client.Fetch
  ( Fetch(..)
  , deriveFetch
  ) where

import           Data.Aeson
import           Data.ByteString.Lazy   (ByteString)
import           Data.Morpheus.Types.IO (GQLRequest (..))
import           Data.Text              (pack)
import           Language.Haskell.TH

class Fetch a where
  type Args a :: *
  __fetch :: (Monad m, FromJSON a) => String -> (ByteString -> m ByteString) -> Args a -> m (Either String a)
  __fetch strQuery trans _variables = eitherDecode <$> trans (encode gqlReq)
    where
      gqlReq = GQLRequest {operationName = Just "<TODO>", query = pack strQuery, variables = Nothing}
  fetch :: (Monad m, FromJSON a) => (ByteString -> m ByteString) -> Args a -> m (Either String a)

deriveFetch :: Type -> Name -> String -> Q [Dec]
deriveFetch argDatatype typeName _query = pure <$> instanceD (cxt []) (appT (conT ''Fetch) (conT typeName)) methods
  where
    methods =
      [ funD (mkName "fetch") [clause [] (normalB [|__fetch query|]) []]
      , pure $ TySynInstD ''Args (TySynEqn [ConT typeName] argDatatype)
      ]
