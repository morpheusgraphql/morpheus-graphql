{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  MultiParamTypeClasses , ScopedTypeVariables, FlexibleInstances , DefaultSignatures, FlexibleContexts #-}

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
                                                , typeOf
                                                )
import           Data.Morpheus.Types.Introspection
                                                ( GQL__InputValue(..)
                                                , createInputValue
                                                , GQLTypeLib
                                                , createType
                                                )
import           Data.Morpheus.Generics.TypeRep
                                                ( Selectors(..) )
import           Data.Morpheus.ErrorMessage    ( requiredArgument
                                                , handleError
                                                )
import Data.Morpheus.Generics.GQLInput        (GQLInput(..))
import Data.Morpheus.Generics.GenericToArgs   (GToArgs(..))
import  qualified Data.Map as M

updateLib :: GQLTypeLib -> GQLTypeLib
updateLib x = x

instance (Selector s, Typeable t , GQLInput t) => Selectors (M1 S s (K1 R t)) GQL__InputValue where
    getFields _ = [ (typeInfo (Proxy:: Proxy  t) name , introInput (Proxy:: Proxy  t))]
      where name = pack $ selName (undefined :: M1 S s (K1 R t) ())

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
    default argsMeta :: (Show p,  Selectors (Rep p) GQL__InputValue , Typeable p) => Proxy p -> [GQL__InputValue]
    argsMeta _ = map fst $ getFields (Proxy :: Proxy (Rep p))

    argsTypes :: Proxy p -> [(GQL__InputValue,GQLTypeLib -> GQLTypeLib)]
    default argsTypes :: (Show p,  Selectors (Rep p) GQL__InputValue , Typeable p) => Proxy p -> [(GQL__InputValue,GQLTypeLib -> GQLTypeLib)]
    argsTypes _ = getFields (Proxy :: Proxy (Rep p))

instance  GQLArgs () where
    fromArgs _ _ = pure ()
    argsMeta _ = []
    argsTypes _ = []


