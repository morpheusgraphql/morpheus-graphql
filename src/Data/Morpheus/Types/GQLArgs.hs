{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.Morpheus.Types.GQLArgs
  ( GQLArgs(..)
  ) where

import           Data.Morpheus.Resolve.Decode               (GDecode (..))
import           Data.Morpheus.Resolve.Generics.TypeRep     (ObjectRep (..), TypeUpdater)
import           Data.Morpheus.Types.Internal.AST.Selection (Arguments)
import           Data.Morpheus.Types.Internal.Data          (DataInputField)
import           Data.Morpheus.Types.Internal.Validation    (Validation)
import           Data.Proxy                                 (Proxy (..))
import           Data.Text                                  (Text)
import           GHC.Generics

class GQLArgs p where
  decode :: Arguments -> Validation p
  default decode :: (Generic p, GDecode Arguments (Rep p)) =>
    Arguments -> Validation p
  decode args = to <$> gDecode "" args
  introspect :: Proxy p -> [((Text, DataInputField), TypeUpdater)]
  default introspect :: ObjectRep (Rep p) () =>
    Proxy p -> [((Text, DataInputField), TypeUpdater)]
  introspect _ = objectFieldTypes (Proxy @(Rep p))

instance GQLArgs () where
  decode _ = pure ()
  introspect _ = []
