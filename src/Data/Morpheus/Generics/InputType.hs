-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes , DefaultSignatures, FlexibleContexts #-}

module Data.Morpheus.Generics.InputType (GQLInput(..)) where

import           Data.Morpheus.Types.Types     (JSType(..),MetaInfo(..))
import           Data.Text                     (Text,unpack)
import           Data.Morpheus.Generics.GenericInputType (GToInput(..),GToEnum(..))
import           GHC.Generics
import           Data.Data

class GQLInput a where
    decode :: JSType -> a
    default decode :: ( Show a  , Generic a, Data a , GToEnum (Rep a) ) => JSType -> a
    decode (JSEnum text) = to $ gToEnum text

instance GQLInput Text where
    decode  (JSString x) = x

instance GQLInput Bool where
    decode  (JSBool x) = x