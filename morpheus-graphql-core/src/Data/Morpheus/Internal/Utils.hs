{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Internal.Utils
  ( capitalize,
    nameSpaceField,
    nameSpaceType,
    capitalTypeName,
    Collection (..),
    Selectable (..),
    Listable (..),
    Merge (..),
    Failure (..),
    KeyOf (..),
    toPair,
    selectBy,
    keys,
    size,
    (<:>),
    mapFst,
    mapSnd,
    mapTuple,
    UpdateT (..),
    resolveUpdates,
    concatUpdates,
    failUpdates,
    ordTraverse,
    ordTraverse_,
    traverseCollection,
    (<.>),
    SemigroupM (..),
    fromListT,
    mergeT,
    runResolutionT,
    ResolutionT,
    prop,
    resolveWith,
    stripFieldNamespace,
    stripConstructorNamespace,
    fromLBS,
    toLBS,
  )
where

import Control.Applicative (Applicative (..))
import Control.Monad ((=<<), (>=>), (>>=), foldM)
import Control.Monad.Reader (MonadReader (..), asks)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader
  ( ReaderT (..),
  )
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Char
  ( toLower,
    toUpper,
  )
import Data.Foldable (foldlM, null, traverse_)
import Data.Function ((&))
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import Data.List (drop, find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (maybe)
import Data.Morpheus.Error.NameCollision (NameCollision (..))
import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName,
    FieldName (..),
    Ref (..),
    Token,
    TypeName (..),
    TypeNameRef (..),
    ValidationErrors,
  )
import Data.Semigroup (Semigroup (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Traversable (traverse)
import Instances.TH.Lift ()
import Prelude
  ( ($),
    (.),
    Bool (..),
    Either (..),
    Eq (..),
    Functor (..),
    Int,
    Maybe (..),
    Monad,
    String,
    const,
    fst,
    length,
    otherwise,
    snd,
  )

toLBS :: Text -> ByteString
toLBS = LB.pack . T.unpack

fromLBS :: ByteString -> Text
fromLBS = LT.toStrict . decodeUtf8

prop :: (b -> b -> m b) -> (a -> b) -> a -> a -> m b
prop f fSel a1 a2 = f (fSel a1) (fSel a2)

nameSpaceType :: [FieldName] -> TypeName -> TypeName
nameSpaceType list (TypeName name) = TypeName . T.concat $ fmap capitalize (fmap readName list <> [name])

nameSpaceField :: TypeName -> FieldName -> FieldName
nameSpaceField nSpace (FieldName name) = FieldName (nonCapital nSpace <> capitalize name)

dropPrefix :: TypeName -> String -> String
dropPrefix (TypeName name) = drop (T.length name)

stripConstructorNamespace :: TypeName -> String -> String
stripConstructorNamespace = dropPrefix

stripFieldNamespace :: TypeName -> String -> String
stripFieldNamespace prefix = uncapitalize . dropPrefix prefix

mapText :: (String -> String) -> Token -> Token
mapText f = T.pack . f . T.unpack

nonCapital :: TypeName -> Token
nonCapital = uncapitalize . readTypeName

class Capitalize a where
  capitalize :: a -> a
  uncapitalize :: a -> a

instance Capitalize String where
  capitalize [] = []
  capitalize (x : xs) = toUpper x : xs
  uncapitalize [] = []
  uncapitalize (x : xs) = toLower x : xs

instance Capitalize Token where
  capitalize = mapText capitalize
  uncapitalize = mapText uncapitalize

capitalTypeName :: FieldName -> TypeName
capitalTypeName = TypeName . capitalize . readName

--(KEY v ~ k) =>
class Collection a coll | coll -> a where
  empty :: coll
  singleton :: a -> coll

instance Collection a [a] where
  empty = []
  singleton x = [x]

instance KeyOf k v => Collection v (HashMap k v) where
  empty = HM.empty
  singleton x = HM.singleton (keyOf x) x

class Selectable k a c | c -> a where
  selectOr :: d -> (a -> d) -> k -> c -> d
  member :: Selectable k a c => k -> c -> Bool
  member = selectOr False (const True)

instance KeyOf k a => Selectable k a [a] where
  selectOr fb f key lib = maybe fb f (find ((key ==) . keyOf) lib)

instance KeyOf k a => Selectable k a (HashMap k a) where
  selectOr fb f key lib = maybe fb f (HM.lookup key lib)

selectBy :: (Failure e m, Selectable k a c, Monad m) => e -> k -> c -> m a
selectBy err = selectOr (failure err) pure

ordTraverse ::
  ( Monad f,
    KeyOf k b,
    Listable a (t a),
    Listable b (t b),
    Failure ValidationErrors f
  ) =>
  (a -> f b) ->
  t a ->
  f (t b)
ordTraverse = traverseCollection

traverseCollection ::
  ( Monad f,
    KeyOf k b,
    Listable a (t a),
    Listable b (t' b),
    Failure ValidationErrors f
  ) =>
  (a -> f b) ->
  t a ->
  f (t' b)
traverseCollection f a = fromElems =<< traverse f (elems a)

ordTraverse_ ::
  ( Monad f,
    Listable a (t a)
  ) =>
  (a -> f b) ->
  t a ->
  f ()
ordTraverse_ f a = traverse_ f (elems a)

class (Eq k, Hashable k) => KeyOf k a | a -> k where
  keyOf :: a -> k

instance (Eq k, Hashable k) => KeyOf k (k, a) where
  keyOf = fst

instance KeyOf FieldName Ref where
  keyOf = refName

instance KeyOf TypeName TypeNameRef where
  keyOf = typeNameRef

toPair :: KeyOf k a => a -> (k, a)
toPair x = (keyOf x, x)

-- list Like Collections
class Listable a coll | coll -> a where
  elems :: coll -> [a]
  fromElems :: (Monad m, Failure ValidationErrors m) => [a] -> m coll

instance (NameCollision a, KeyOf k a) => Listable a (HashMap k a) where
  fromElems xs = runResolutionT (fromListT xs) hmUnsafeFromValues failOnDuplicates
  elems = HM.elems

keys :: (KeyOf k a, Listable a coll) => coll -> [k]
keys = fmap keyOf . elems

size :: Listable a coll => coll -> Int
size = length . elems

-- Merge Object with of Failure as an Option
class Merge a where
  merge :: (Monad m, Failure ValidationErrors m) => [Ref] -> a -> a -> m a

instance (NameCollision a, KeyOf k a) => Merge (HashMap k a) where
  merge _ x y = runResolutionT (fromListT $ HM.elems x <> HM.elems y) hmUnsafeFromValues failOnDuplicates

(<:>) :: (Monad m, Merge a, Failure ValidationErrors m) => a -> a -> m a
(<:>) = merge []

class SemigroupM m a where
  mergeM :: [Ref] -> a -> a -> m a

(<.>) ::
  (SemigroupM m a) =>
  a ->
  a ->
  m a
(<.>) = mergeM []

-- Failure: for custom Morpheus GrapHQL errors
class Applicative f => Failure error (f :: * -> *) where
  failure :: error -> f v

instance Failure error (Either error) where
  failure = Left

instance (Monad m, Failure errors m) => Failure errors (ReaderT ctx m) where
  failure = lift . failure

mapFst :: (a -> a') -> (a, b) -> (a', b)
mapFst f (a, b) = (f a, b)

mapSnd :: (b -> b') -> (a, b) -> (a, b')
mapSnd f (a, b) = (a, f b)

mapTuple :: (a -> a') -> (b -> b') -> (a, b) -> (a', b')
mapTuple f1 f2 (a, b) = (f1 a, f2 b)

-- Helper Functions
newtype UpdateT m a = UpdateT {updateTState :: a -> m a}

failUpdates :: (Failure e m) => e -> UpdateT m a
failUpdates = UpdateT . const . failure

concatUpdates :: Monad m => [UpdateT m a] -> UpdateT m a
concatUpdates x = UpdateT (`resolveUpdates` x)

resolveUpdates :: Monad m => a -> [UpdateT m a] -> m a
resolveUpdates a = foldM (&) a . fmap updateTState

type RESOLUTION k a coll m =
  ( Monad m,
    KeyOf k a,
    Listable a coll
  )

data Resolution a coll m = Resolution
  { resolveDuplicates :: NonEmpty a -> m a,
    fromNoDuplicates :: [a] -> coll
  }

runResolutionT ::
  ResolutionT a coll m b ->
  ([a] -> coll) ->
  (NonEmpty a -> m a) ->
  m b
runResolutionT (ResolutionT x) fromNoDuplicates resolveDuplicates = runReaderT x Resolution {..}

newtype ResolutionT a coll m x = ResolutionT
  { _runResolutionT :: ReaderT (Resolution a coll m) m x
  }
  deriving
    ( Functor,
      Monad,
      Applicative,
      MonadReader (Resolution a coll m)
    )

instance MonadTrans (ResolutionT e coll) where
  lift = ResolutionT . lift

instance
  ( Monad m,
    Failure ValidationErrors m
  ) =>
  Failure ValidationErrors (ResolutionT a coll m)
  where
  failure = lift . failure

resolveDuplicatesM :: Monad m => NonEmpty a -> ResolutionT a coll m a
resolveDuplicatesM xs = asks resolveDuplicates >>= lift . (xs &)

fromNoDuplicatesM :: Monad m => [a] -> ResolutionT a coll m coll
fromNoDuplicatesM xs = asks ((xs &) . fromNoDuplicates)

class UpdateByKey k a c | c -> a, c -> k where
  updatByKey :: k -> (Maybe a -> a) -> c -> c

instance (Eq k, Hashable k) => UpdateByKey k a [(k, a)] where
  updatByKey key f values
    | key `member` values = fmap replaceBy values
    | otherwise = values <> [(key, f Nothing)]
    where
      replaceBy (entryKey, entryValue)
        | key == entryKey = (key, f (Just entryValue))
        | otherwise = (entryKey, entryValue)

insertWithList :: (Eq k, Hashable k) => (k, NonEmpty a) -> [(k, NonEmpty a)] -> [(k, NonEmpty a)]
insertWithList (key, value) = updatByKey key updater
  where
    updater Nothing = value
    updater (Just x) = x <> value

clusterDuplicates :: (Eq k, Hashable k) => [(k, NonEmpty a)] -> [(k, a)] -> [(k, NonEmpty a)]
clusterDuplicates collected [] = collected
clusterDuplicates coll ((key, value) : xs) = clusterDuplicates (insertWithList (key, value :| []) coll) xs

fromListDuplicates :: (KeyOf k a) => [a] -> [(k, NonEmpty a)]
fromListDuplicates xs = clusterDuplicates [] (fmap toPair xs)

fromListT ::
  RESOLUTION k a coll m =>
  [a] ->
  ResolutionT a coll m coll
fromListT = traverse (resolveDuplicatesM . snd) . fromListDuplicates >=> fromNoDuplicatesM

mergeT :: RESOLUTION k a coll m => coll -> coll -> ResolutionT a coll m coll
mergeT c1 c2 = traverse (resolveDuplicatesM . snd) (fromListDuplicates (elems c1 <> elems c2)) >>= fromNoDuplicatesM

resolveWith ::
  Monad m =>
  (a -> a -> m a) ->
  NonEmpty a ->
  m a
resolveWith f (x :| xs) = foldlM f x xs

hmUnsafeFromValues :: (Eq k, KeyOf k a) => [a] -> HashMap k a
hmUnsafeFromValues = HM.fromList . fmap toPair

failOnDuplicates :: (Failure ValidationErrors m, NameCollision a) => NonEmpty a -> m a
failOnDuplicates (x :| xs)
  | null xs = pure x
  | otherwise = failure $ fmap nameCollision (x : xs)
