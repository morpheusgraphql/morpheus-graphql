{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators  , TypeApplications , FlexibleContexts , MultiParamTypeClasses , DeriveAnyClass , AllowAmbiguousTypes , DefaultSignatures , ConstraintKinds #-}

module Data.Morpheus
  ( interpreter
  , eitherToResponse
  , GQLResponse
  , GQLSelection
  , GQLQuery
  , GQLArgs
  , (::->)(..)
  , GQLRequest(..)
  , ResolveIO
  , GQLInput
  , EnumOf(unpackEnum)
  , GQLEnum
  , GQLRoot(..)
  , GQLMutation(..)
  , NoMutation(..)
  ) where

import           Control.Monad.Trans.Except          (ExceptT (..), runExceptT)
import           Data.Aeson                          (decode)
import qualified Data.ByteString.Lazy.Char8          as B
import           Data.Morpheus.Error.Utils           (renderErrors)
import           Data.Morpheus.ErrorMessage          (errorMessage)
import           Data.Morpheus.Generics.GQLArgs      (GQLArgs)
import           Data.Morpheus.Generics.GQLEnum      (GQLEnum)
import           Data.Morpheus.Generics.GQLInput     (GQLInput)
import           Data.Morpheus.Generics.GQLMutation  (GQLMutation (..), NoMutation (..))
import           Data.Morpheus.Generics.GQLQuery     (GQLQuery (..))
import           Data.Morpheus.Generics.GQLSelection (GQLSelection)
import           Data.Morpheus.Parser.Parser         (parseGQL, parseLineBreaks)
import           Data.Morpheus.PreProcess.PreProcess (preProcessQuery)
import           Data.Morpheus.Types.Introspection   (GQLTypeLib)
import           Data.Morpheus.Types.JSType          (JSType)
import           Data.Morpheus.Types.Types           ((::->) (Resolver), EnumOf (unpackEnum),
                                                      GQLOperator (..), GQLRequest (..),
                                                      GQLResponse (..), ResolveIO, failResolveIO)
import           Data.Text                           (pack)
import           Data.Proxy

data GQLRoot a b = GQLRoot
  { queryResolver    :: a
  , mutationResolver :: b
  }

class AsGQL a f where
  cool :: Proxy a -> f  -> String
  default cool :: (Show f ) => Proxy a -> f -> String
  cool _ = show

class AsInt a where
  isType :: a -> Proxy Int
  default isType :: a -> Proxy Int
  isType _ = Proxy @Int

data O = O {
  o :: Int,
  i:: Int
} deriving (Show, AsInt , AsGQL Int)

getO :: O
getO = O { o = 1 , i = 1 }

showO :: String
showO = cool (isType getO) getO

schema :: (GQLQuery a, GQLMutation b) => a -> b -> GQLTypeLib
schema queryRes mutationRes = querySchema queryRes $ mutationSchema mutationRes

resolve :: (GQLQuery a, GQLMutation b) => GQLRoot a b -> GQLRequest -> ResolveIO JSType
resolve rootResolver body = do
  rootGQL <- ExceptT $ pure (parseGQL body >>= preProcessQuery gqlSchema)
  case rootGQL of
    QueryOperator _ selection       -> encodeQuery queryRes gqlSchema selection
    MutationOperator _ selection -> encodeMutation mutationRes selection
  where
    gqlSchema = schema queryRes mutationRes
    queryRes = queryResolver rootResolver
    mutationRes = mutationResolver rootResolver

lineBreaks :: B.ByteString -> [Int]
lineBreaks req =
  case decode req of
    Just x  -> parseLineBreaks x
    Nothing -> []

interpreter :: (GQLQuery a, GQLMutation b) => GQLRoot a b -> B.ByteString -> IO GQLResponse
interpreter rootResolver request = do
  value <- runExceptT $ parseRequest request >>= resolve rootResolver
  case value of
    Left x  -> pure $ Errors $ renderErrors (lineBreaks request) x
    Right x -> pure $ Data x

eitherToResponse :: (a -> a) -> Either String a -> ResolveIO a
eitherToResponse _ (Left x)  = failResolveIO $ errorMessage 0 (pack $ show x)
eitherToResponse f (Right x) = pure (f x)

parseRequest :: B.ByteString -> ResolveIO GQLRequest
parseRequest _ = failResolveIO $ errorMessage 0 (pack showO)
  --case decode text of
   -- Just x  -> failResolveIO $ errorMessage 0 (pack showO)
   -- Nothing -> failResolveIO $ errorMessage 0 (pack showO)
