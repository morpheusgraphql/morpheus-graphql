{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE  MultiParamTypeClasses , ScopedTypeVariables, FlexibleInstances , DefaultSignatures, FlexibleContexts #-}

module Data.Morpheus.Generics.GQLArgs
    ( GQLArgs(..)
    )
where

import           GHC.Generics
import qualified Data.Text  as  T
import qualified Data.Data  as  D
import qualified Data.Map as M
import           Data.Morpheus.Generics.TypeRep ( Selectors(..) )
import           Data.Proxy                     ( Proxy(..) )
import Data.Morpheus.Generics.GQLInput        (GQLInput(..))
import Data.Morpheus.Generics.GenericToArgs   (GToArgs(..))
import qualified  Data.Morpheus.ErrorMessage    as Err
import           Data.Morpheus.Types.Types     ( Arguments
                                                , Validation(..)
                                                , (::->)(Some, None)
                                                , Argument(..)
                                                )
import Data.Morpheus.Types.MetaInfo (MetaInfo(..), initialMeta)
import Data.Morpheus.Types.JSType (JSType(..))
import           Data.Morpheus.Types.Introspection
                                                ( GQL__InputValue(..)
                                                , createInputValue
                                                , GQLTypeLib
                                                , createType
                                                )

updateLib :: GQLTypeLib -> GQLTypeLib
updateLib x = x

instance (Selector s, D.Typeable t , GQLInput t) => Selectors (M1 S s (K1 R t)) GQL__InputValue where
    getFields _ = [ (typeInfo (Proxy:: Proxy  t) name , introInput (Proxy:: Proxy  t))]
      where name = T.pack $ selName (undefined :: M1 S s (K1 R t) ())

instance GQLInput a => GToArgs  (K1 i a)  where
    gToArgs meta args =
        case lookup (key meta) args of
            Nothing -> Left $ Err.requiredArgument meta
            Just (Argument x) -> K1 <$> decode x
            Just x -> Err.handleError $ T.pack $ show x

class GQLArgs p where
    decodeArgs :: Arguments -> Maybe p -> Validation p
    default decodeArgs :: ( Show p , Generic p, D.Data p , GToArgs (Rep p) ) => Arguments -> Maybe p -> Validation p
    decodeArgs args _ = to <$> gToArgs initialMeta args

    introspectArgs :: Proxy p -> [(GQL__InputValue,GQLTypeLib -> GQLTypeLib)]
    default introspectArgs :: (Show p,  Selectors (Rep p) GQL__InputValue , D.Typeable p) => Proxy p -> [(GQL__InputValue,GQLTypeLib -> GQLTypeLib)]
    introspectArgs _ = getFields (Proxy :: Proxy (Rep p))

instance  GQLArgs () where
    decodeArgs _ _ = pure ()
    introspectArgs _ = []


