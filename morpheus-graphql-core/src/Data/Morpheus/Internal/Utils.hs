{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
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
    ordTraverse,
    traverseCollection,
    (<.>),
    SemigroupM (..),
    prop,
    stripFieldNamespace,
    stripConstructorNamespace,
    fromLBS,
    toLBS,
    mergeT,
    concatTraverse,
    join,
  )
where

import Control.Applicative (Applicative (..))
import Control.Monad ((=<<), (>>=))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Reader
  ( ReaderT (..),
  )
import Data.ByteString.Lazy (ByteString)
import Data.Char
  ( toLower,
    toUpper,
  )
import Data.Functor ((<$>), Functor (..))
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Hashable (Hashable)
import Data.List (drop, find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (maybe)
import Data.Morpheus.Error.NameCollision (NameCollision (..))
import Data.Morpheus.Ext.Map
  ( Indexed (..),
    ResolutionT,
    fromListT,
    runResolutionT,
  )
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
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Data.Traversable (traverse)
import Instances.TH.Lift ()
import Prelude
  ( ($),
    (.),
    Bool (..),
    Either (..),
    Eq (..),
    Foldable (..),
    Int,
    Monad,
    String,
    Traversable,
    const,
    fst,
    length,
    otherwise,
  )

toLBS :: Text -> ByteString
toLBS = encodeUtf8 . LT.fromStrict

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

  member :: k -> c -> Bool
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
    Failure ValidationErrors f,
    Traversable t
  ) =>
  (a -> f b) ->
  t a ->
  f (t b)
ordTraverse = traverse

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

mergeT :: (KeyOf k a, Monad m, Listable a c) => c -> c -> ResolutionT k a c m c
mergeT x y = fromListT (toPair <$> (elems x <> elems y))

instance (NameCollision a, KeyOf k a) => Listable a (HashMap k a) where
  fromElems xs = runResolutionT (fromListT (toPair <$> xs)) HM.fromList failOnDuplicates
  elems = HM.elems

concatTraverse ::
  ( Monad m,
    Failure ValidationErrors m,
    Listable a ca,
    Collection b cb,
    Merge cb
  ) =>
  (a -> m cb) ->
  ca ->
  m cb
concatTraverse f smap =
  traverse f (elems smap)
    >>= join

join ::
  ( Collection e a,
    Monad m,
    Failure ValidationErrors m,
    Merge a
  ) =>
  [a] ->
  m a
join = __join empty
  where
    __join acc [] = pure acc
    __join acc (x : xs) = acc <:> x >>= (`__join` xs)

keys :: (KeyOf k a, Listable a coll) => coll -> [k]
keys = fmap keyOf . elems

size :: Listable a coll => coll -> Int
size = length . elems

-- Merge Object with of Failure as an Option
class Merge a where
  merge :: (Monad m, Failure ValidationErrors m) => [Ref] -> a -> a -> m a

instance (NameCollision a, KeyOf k a) => Merge (HashMap k a) where
  merge _ x y = runResolutionT (fromListT $ HM.toList x <> HM.toList y) HM.fromList failOnDuplicates

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

failOnDuplicates :: (Failure ValidationErrors m, NameCollision a) => NonEmpty a -> m a
failOnDuplicates (x :| xs)
  | null xs = pure x
  | otherwise = failure $ fmap nameCollision (x : xs)

instance (Eq k, Hashable k) => KeyOf k (Indexed k a) where
  keyOf = indexedKey
