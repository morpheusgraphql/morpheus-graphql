{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Internal.Utils
  ( camelCaseTypeName,
    camelCaseFieldName,
    Collection (..),
    Selectable (..),
    FromElems (..),
    Failure (..),
    KeyOf (..),
    toPair,
    selectBy,
    traverseCollection,
    prop,
    stripFieldNamespace,
    stripConstructorNamespace,
    fromLBS,
    toLBS,
    mergeT,
    Elems (..),
    size,
    failOnDuplicates,
    Empty (..),
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Char
  ( toLower,
  )
import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.Error.NameCollision (NameCollision (..))
import Data.Morpheus.Ext.Elems (Elems (..), size)
import Data.Morpheus.Ext.Empty
import Data.Morpheus.Ext.Failure (Failure (..))
import Data.Morpheus.Ext.KeyOf (KeyOf (..), toPair)
import Data.Morpheus.Ext.Map
  ( ResolutionT,
    fromListT,
    runResolutionT,
  )
import Data.Morpheus.Ext.Selectable
import Data.Morpheus.Types.Internal.AST.Base
  ( ValidationErrors,
  )
import Data.Morpheus.Types.Internal.AST.Name
  ( Name (..),
    TypeName,
    camelCaseFieldName,
    camelCaseTypeName,
  )
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Instances.TH.Lift ()
import Relude hiding
  ( ByteString,
    decodeUtf8,
    encodeUtf8,
  )

toLBS :: Text -> ByteString
toLBS = encodeUtf8 . LT.fromStrict

fromLBS :: ByteString -> Text
fromLBS = LT.toStrict . decodeUtf8

prop :: (b -> b -> m b) -> (a -> b) -> a -> a -> m b
prop f fSel a1 a2 = f (fSel a1) (fSel a2)

dropPrefix :: TypeName -> String -> String
dropPrefix name = drop (T.length $ unpackName name)

stripConstructorNamespace :: TypeName -> String -> String
stripConstructorNamespace = dropPrefix

stripFieldNamespace :: TypeName -> String -> String
stripFieldNamespace prefix = __uncapitalize . dropPrefix prefix
  where
    __uncapitalize [] = []
    __uncapitalize (x : xs) = toLower x : xs

--(KEY v ~ k) =>
class Collection a coll | coll -> a where
  singleton :: a -> coll

instance Collection a [a] where
  singleton x = [x]

instance Collection a (NonEmpty a) where
  singleton x = x :| []

instance KeyOf k v => Collection v (HashMap k v) where
  singleton x = HM.singleton (keyOf x) x

traverseCollection ::
  ( Monad f,
    KeyOf k b,
    Elems a (t a),
    FromElems f b (t' b),
    Failure ValidationErrors f
  ) =>
  (a -> f b) ->
  t a ->
  f (t' b)
traverseCollection f a = fromElems =<< traverse f (elems a)

-- list Like Collections
class FromElems m a coll | coll -> a where
  fromElems :: [a] -> m coll

mergeT :: (KeyOf k a, Monad m, Elems a c) => c -> c -> ResolutionT k a c m c
mergeT x y = fromListT (toPair <$> (elems x <> elems y))

instance
  ( NameCollision a,
    Failure ValidationErrors m,
    KeyOf k a,
    Monad m
  ) =>
  FromElems m a (HashMap k a)
  where
  fromElems xs = runResolutionT (fromListT (toPair <$> xs)) HM.fromList failOnDuplicates

-- Merge Object with of Failure as an Option
failOnDuplicates :: (Failure ValidationErrors m, NameCollision a) => NonEmpty a -> m a
failOnDuplicates (x :| xs)
  | null xs = pure x
  | otherwise = failure $ fmap nameCollision (x : xs)
