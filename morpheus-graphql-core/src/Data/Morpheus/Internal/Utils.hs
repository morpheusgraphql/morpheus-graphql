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
    FromElems (..),
    Failure (..),
    KeyOf (..),
    toPair,
    selectBy,
    mapFst,
    mapSnd,
    mapTuple,
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
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.Char
  ( toLower,
    toUpper,
  )
import qualified Data.HashMap.Lazy as HM
import Data.Morpheus.Error.NameCollision (NameCollision (..))
import Data.Morpheus.Ext.Elems (Elems (..), size)
import Data.Morpheus.Ext.Failure (Failure (..))
import Data.Morpheus.Ext.KeyOf (KeyOf (..), toPair)
import Data.Morpheus.Ext.Map
  ( ResolutionT,
    fromListT,
    runResolutionT,
  )
import Data.Morpheus.Types.Internal.AST.Base
  ( FieldName,
    FieldName (..),
    Token,
    TypeName (..),
    ValidationErrors,
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

nameSpaceType :: [FieldName] -> TypeName -> TypeName
nameSpaceType list (TypeName name) = TypeName . T.concat $ fmap capitalize (fmap readName list <> [name])

nameSpaceField :: TypeName -> FieldName -> FieldName
nameSpaceField (TypeName nSpace) (FieldName name) = FieldName (uncapitalize nSpace <> capitalize name)

dropPrefix :: TypeName -> String -> String
dropPrefix (TypeName name) = drop (T.length name)

stripConstructorNamespace :: TypeName -> String -> String
stripConstructorNamespace = dropPrefix

stripFieldNamespace :: TypeName -> String -> String
stripFieldNamespace prefix = __uncapitalize . dropPrefix prefix
  where
    __uncapitalize [] = []
    __uncapitalize (x : xs) = toLower x : xs

capitalize :: Text -> Text
capitalize = mapFstChar toUpper

uncapitalize :: Text -> Text
uncapitalize = mapFstChar toLower

mapFstChar :: (Char -> Char) -> Token -> Token
mapFstChar f x
  | T.null x = x
  | otherwise = T.singleton (f $ T.head x) <> T.tail x

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
