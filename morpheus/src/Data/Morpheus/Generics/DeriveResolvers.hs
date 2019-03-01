{-# LANGUAGE  FlexibleInstances , TypeOperators #-}

module Data.Morpheus.Generics.DeriveResolvers
    ( DeriveResolvers(..) , resolveBySelection )
where

import           GHC.Generics
import           Data.Maybe                     (fromMaybe)
import qualified Data.Text                   as T
import           Data.Morpheus.Types.Types      ( QuerySelection
                                                , ResolveIO
                                                )
import           Data.Morpheus.Types.MetaInfo   ( MetaInfo(..) )
import           Data.Morpheus.Types.JSType     ( JSType(..) )

-- type D1 = M1 D
-- type C1 = M1 C
-- type S1 = M1 S
-- M1 : Meta-information (constructor names, etc.)
-- D  :Datatype : Class for dataTypes that represent dataTypes
-- C :Constructor :
-- S - Selector: Class for dataTypes that represent records
-- Rep = D1 (...)  (C1 ...) (S1 (...) :+: D1 (...)  (C1 ...) (S1 (...)

unwrapMonadTuple :: Monad m => (T.Text, m a) -> m (T.Text, a)
unwrapMonadTuple (text, ioa) = ioa >>= \x -> pure (text, x)

selectResolver:: [(T.Text, QuerySelection -> ResolveIO JSType )] -> (T.Text,QuerySelection) -> ResolveIO (T.Text,  JSType)
selectResolver x (key, gql ) = unwrapMonadTuple (key, (fromMaybe  (\x-> pure JSNull) $ lookup key x) gql )

resolveBySelection :: [(T.Text,QuerySelection)] ->  [(T.Text, QuerySelection -> ResolveIO JSType)] -> ResolveIO JSType
resolveBySelection selection resolvers = JSObject <$> mapM (selectResolver resolvers) selection

class DeriveResolvers f where
    deriveResolvers:: MetaInfo -> f a -> [(T.Text, QuerySelection -> ResolveIO JSType)]

instance DeriveResolvers U1  where
    deriveResolvers  _  _ = []

instance (Selector s, DeriveResolvers f) => DeriveResolvers (M1 S s f) where
    deriveResolvers meta m@(M1 src) = deriveResolvers (meta{ key = T.pack $ selName m})  src

instance (Datatype c, DeriveResolvers f) => DeriveResolvers (M1 D c f)  where
    deriveResolvers meta m@(M1 src) = deriveResolvers (meta{ className = T.pack $ datatypeName m}) src

instance (Constructor c  , DeriveResolvers f) => DeriveResolvers (M1 C c f)  where
    deriveResolvers meta m@(M1 src) =  deriveResolvers (meta{ cons = T.pack $ conName m}) src

instance (DeriveResolvers f , DeriveResolvers g ) => DeriveResolvers (f :*: g)  where
    deriveResolvers meta (a :*: b) = deriveResolvers meta a ++ deriveResolvers meta b
