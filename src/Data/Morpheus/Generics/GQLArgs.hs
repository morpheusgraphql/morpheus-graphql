{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  ScopedTypeVariables, DefaultSignatures, FlexibleContexts #-}

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
import Data.Morpheus.Generics.InputType        (GQLInput(..))
import Data.Morpheus.Generics.GenericToArgs   (GToArgs(..))

instance GQLInput a => GToArgs  (K1 i a)  where
    gToArgs meta args =
        case lookup (key meta) args of
            Nothing -> Left $ requiredArgument meta
            Just (Argument x) -> pure $ K1 $ decode x
            Just x -> handleError $ pack $ show x

class GQLArgs p where
    fromArgs :: Arguments -> Maybe p -> Eval p
    default fromArgs :: ( Show p , Generic p, Data p , GToArgs (Rep p) ) => Arguments -> Maybe p -> Eval p
    fromArgs args _ = to <$> gToArgs (MetaInfo "" "" "") args

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

instance  GQLArgs (Maybe a) where
    fromArgs _ (Just t) = pure t
    argsMeta _ = []

instance  GQLArgs () where
    fromArgs _ _ = pure ()
    argsMeta _ = []
