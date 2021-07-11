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
    IsMap (..),
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
    Empty (..),
    elems,
    HistoryT,
    addPath,
    startHistory,
    mergeConcat,
    (<:>),
    selectOr,
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Char
  ( toLower,
  )
import qualified Data.HashMap.Lazy as HM
import Data.Mergeable
  ( Merge (merge),
    NameCollision (..),
    ResolutionT,
    fromListT,
    mergeConcat,
    mergeNoDuplicates,
  )
import Data.Mergeable.IsMap
import Data.Morpheus.Ext.Empty
import Data.Morpheus.Ext.Failure (Failure (..))
import Data.Morpheus.Ext.KeyOf (KeyOf (..), toPair)
import Data.Morpheus.Types.Internal.AST.Base (Ref)
import Data.Morpheus.Types.Internal.AST.Error
  ( ValidationErrors,
  )
import Data.Morpheus.Types.Internal.AST.Name
  ( FieldName,
    Name (..),
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

(<:>) :: (Merge (HistoryT m) a, Monad m) => a -> a -> m a
x <:> y = startHistory (merge x y)

addPath :: MonadReader [a1] m => a1 -> m a2 -> m a2
addPath p = local (\xs -> xs <> [p])

type HistoryT = ReaderT [Ref FieldName]

startHistory :: HistoryT m a -> m a
startHistory x = runReaderT x []

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

{-# DEPRECATED elems "use Foldable.toList" #-}
elems :: Foldable t => t a -> [a]
elems = toList

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
    FromElems f b (t' b),
    Failure ValidationErrors f,
    Foldable t
  ) =>
  (a -> f b) ->
  t a ->
  f (t' b)
traverseCollection f a = fromElems =<< traverse f (toList a)

-- list Like Collections
class FromElems m a coll | coll -> a where
  fromElems :: [a] -> m coll

mergeT :: (KeyOf k a, Foldable t, Monad m) => t a -> t a -> ResolutionT k a c m c
mergeT x y = fromListT (toPair <$> (toList x <> toList y))

instance
  ( NameCollision a,
    Failure ValidationErrors m,
    KeyOf k a,
    Monad m
  ) =>
  FromElems m a (HashMap k a)
  where
  fromElems xs = mergeNoDuplicates HM.fromList (toPair <$> xs)
