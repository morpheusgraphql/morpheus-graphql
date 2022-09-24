{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Utils.Proxy
  ( conNameProxy,
    isRecordProxy,
    selNameProxy,
    symbolName,
    ContextValue (..),
  )
where

import Data.Morpheus.Kind (DerivingKind)
import Data.Morpheus.Server.Types.Internal
  ( GQLTypeOptions (..),
  )
import Data.Morpheus.Types.Internal.AST
  ( FieldName,
    TypeName,
    packName,
  )
import Data.Text
  ( pack,
  )
import qualified Data.Text as T
import GHC.Generics
  ( C,
    Constructor,
    M1 (..),
    Meta,
    S,
    Selector,
    U1 (..),
    conIsRecord,
    conName,
    selName,
  )
import GHC.TypeLits
import Relude hiding (undefined)
import Prelude (undefined)

conNameProxy :: forall f (c :: Meta). Constructor c => GQLTypeOptions -> f c -> TypeName
conNameProxy options _ =
  packName $ pack $ constructorTagModifier options $ conName (undefined :: M1 C c U1 a)

selNameProxy :: forall f (s :: Meta). Selector s => GQLTypeOptions -> f s -> FieldName
selNameProxy options _ =
  fromHaskellName $
    fieldLabelModifier options $
      selName (undefined :: M1 S s f a)

fromHaskellName :: String -> FieldName
fromHaskellName hsName
  | not (null hsName) && (T.last name == '\'') = packName (T.init name)
  | otherwise = packName name
  where
    name = T.pack hsName
{-# INLINE fromHaskellName #-}

isRecordProxy :: forall f (c :: Meta). Constructor c => f c -> Bool
isRecordProxy _ = conIsRecord (undefined :: (M1 C c f a))

symbolName :: KnownSymbol a => f a -> FieldName
symbolName = fromString . symbolVal

newtype ContextValue (kind :: DerivingKind) a = ContextValue
  { unContextValue :: a
  }