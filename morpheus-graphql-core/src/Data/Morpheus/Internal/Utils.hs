{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Internal.Utils
  ( camelCaseTypeName,
    camelCaseFieldName,
    singleton,
    IsMap,
    Failure,
    failure,
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
    member,
    unsafeFromList,
    insert,
    fromElems,
    throwMany,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.ByteString.Lazy (ByteString)
import Data.Char
  ( toLower,
  )
import Data.Mergeable
  ( IsMap,
    Merge (merge),
    NameCollision (..),
    ResolutionT,
    fromListT,
    mergeConcat,
    throwMany,
  )
import Data.Mergeable.IsMap (FromList (..), member, selectBy, selectOr, unsafeFromList)
import qualified Data.Mergeable.IsMap as M
import Data.Mergeable.SafeHashMap (SafeHashMap)
import Data.Morpheus.Ext.Empty
import Data.Morpheus.Ext.KeyOf (KeyOf (..), toPair)
import Data.Morpheus.Types.Internal.AST.Base (Ref)
import Data.Morpheus.Types.Internal.AST.Error
  ( ValidationError,
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
    fromList,
  )

{-# DEPRECATED Failure "use MonadError" #-}

type Failure = MonadError

{-# DEPRECATED failure "use throwError" #-}
failure :: MonadError e m => e -> m a
failure = throwError

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

singleton :: (IsMap k m, KeyOf k a) => a -> m a
singleton x = M.singleton (keyOf x) x

traverseCollection ::
  ( Monad m,
    Failure ValidationError m,
    KeyOf k b,
    FromList m map k b,
    Foldable t
  ) =>
  (a -> m b) ->
  t a ->
  m (map k b)
traverseCollection f a = fromElems =<< traverse f (toList a)

fromElems ::
  ( Monad m,
    KeyOf k a,
    FromList m map k a
  ) =>
  [a] ->
  m (map k a)
fromElems = fromList . map toPair

insert ::
  ( NameCollision e a,
    KeyOf k a,
    Failure e m
  ) =>
  a ->
  SafeHashMap k a ->
  m (SafeHashMap k a)
insert x = merge (singleton x)

mergeT :: (KeyOf k a, Foldable t, Monad m) => t a -> t a -> ResolutionT k a c m c
mergeT x y = fromListT (toPair <$> (toList x <> toList y))
