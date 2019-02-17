{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators , ScopedTypeVariables, DefaultSignatures, FlexibleContexts, FlexibleInstances #-}

module Data.Morpheus.Generics.GQLArgs
    ( GQLArgs(..)
    )
where

import           Data.Text                      ( Text(..)
                                                , pack
                                                )
import           GHC.Generics
import           Data.Morpheus.Types.Types     ( Arguments
                                                , Eval(..)
                                                , (::->)(Some, None)
                                                , MetaInfo(..)
                                                , Argument(..)
                                                , JSType(..)
                                                )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Data                      ( Typeable
                                                , Data
                                                )
import           Data.Morpheus.Types.Introspection
                                                ( GQL__InputValue(..)
                                                , createInputValue
                                                )
import           Data.Morpheus.Generics.TypeRep
                                                ( ArgsMeta(..) )
import           Data.Morpheus.ErrorMessage    ( requiredArgument
                                                , handleError
                                                )

fixProxy :: (a -> f a) -> f a
fixProxy f = f undefined

initMeta = MetaInfo { className = "", cons = "", key = "" }

class InputValue a where
    decode :: JSType -> a

instance InputValue Text where
    decode  (JSString x) = x

instance InputValue Bool where
    decode  (JSBool x) = x

class GToArgs f where
    gToArgs :: MetaInfo -> Arguments -> Eval (f a)

instance GToArgs U1  where
    gToArgs _ _ = pure U1

instance InputValue a => GToArgs  (K1 i a)  where
    gToArgs meta args =
        case lookup (key meta) args of
            Nothing -> Left $ requiredArgument meta
            Just (Argument x) -> pure $ K1 $ decode x
            Just x -> handleError $ pack $ show x

instance (Selector c, GToArgs f ) => GToArgs (M1 S c f) where
    gToArgs meta gql = fixProxy (\x -> M1 <$> gToArgs (meta{ key = pack $ selName x}) gql)

instance (Datatype c, GToArgs f)  => GToArgs (M1 D c f)  where
    gToArgs meta gql  = fixProxy(\x -> M1 <$> gToArgs (meta {className = pack $ datatypeName x}) gql)

instance GToArgs f  => GToArgs (M1 C c f)  where
    gToArgs meta gql  = M1 <$> gToArgs meta gql

instance (GToArgs f , GToArgs g ) => GToArgs (f :*: g)  where
    gToArgs meta gql = (:*:) <$> gToArgs meta gql <*> gToArgs meta gql

class GQLArgs p where
    fromArgs :: Arguments -> Maybe p -> Eval p
    default fromArgs :: ( Show p , Generic p, Data p , GToArgs (Rep p) ) => Arguments -> Maybe p -> Eval p
    fromArgs args _ = to <$> gToArgs initMeta args

    argsMeta :: Proxy p -> [GQL__InputValue]
    default argsMeta :: (Show p, ArgsMeta (Rep p) , Typeable p) => Proxy p -> [GQL__InputValue]
    argsMeta _ = map mapValue $ getMeta (Proxy :: Proxy (Rep p))
        where mapValue (x,y) = createInputValue x y

instance  GQLArgs Text where
    fromArgs _ (Just t) = pure t
    fromArgs _ _ = pure "Nothing found"
    argsMeta _ = []

instance  GQLArgs Bool where
    fromArgs _ (Just t) = pure t
    fromArgs _ _ = pure False
    argsMeta _ = [GQL__InputValue { name = "Boolean", description = "", _type = Nothing, defaultValue = "" }]

instance  GQLArgs () where
    fromArgs _ _ = pure ()
    argsMeta _ = []
