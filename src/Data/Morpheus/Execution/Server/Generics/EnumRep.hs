{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Morpheus.Execution.Server.Generics.EnumRep
  ( EnumRep(..)
  )
where

import           Data.Proxy                     ( Proxy(..) )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           GHC.Generics

-- MORPHEUS
import           Data.Morpheus.Error.Internal   ( internalError )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Eventless )

class EnumRep f where
  encodeRep :: f a -> Text
  decodeEnum :: Text -> Eventless (f a)
  enumTags :: Proxy f -> [Text]

instance (Datatype c, EnumRep f) => EnumRep (M1 D c f) where
  encodeRep (M1 src) = encodeRep src
  decodeEnum = fmap M1 . decodeEnum
  enumTags _ = enumTags (Proxy @f)

instance (Constructor c) => EnumRep (M1 C c U1) where
  encodeRep m@(M1 _) = pack $ conName m
  decodeEnum _ = pure $ M1 U1
  enumTags _ = [pack $ conName (undefined :: (M1 C c U1 x))]

instance (EnumRep a, EnumRep b) => EnumRep (a :+: b) where
  encodeRep (L1 x) = encodeRep x
  encodeRep (R1 x) = encodeRep x
  decodeEnum name
    | name `elem` enumTags (Proxy @a) = L1 <$> decodeEnum name
    | name `elem` enumTags (Proxy @b) = R1 <$> decodeEnum name
    | otherwise = internalError
      ("Constructor \"" <> name <> "\" could not find in Union")
  enumTags _ = enumTags (Proxy @a) ++ enumTags (Proxy @b)
