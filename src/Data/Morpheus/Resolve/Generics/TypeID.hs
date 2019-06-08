{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Resolve.Generics.TypeID
  ( TypeID(..)
  ) where

import           Data.Text    (Text, pack)
import           GHC.Generics

class TypeID f where
  typeNameOf :: f a -> Text
  typeIDOf :: f a -> Text

instance Datatype c => TypeID (M1 D c f) where
  typeNameOf m@(M1 _) = pack (datatypeName m)
  typeIDOf m@(M1 _) = pack (moduleName m ++ "." ++ packageName m ++ "." ++ datatypeName m)
