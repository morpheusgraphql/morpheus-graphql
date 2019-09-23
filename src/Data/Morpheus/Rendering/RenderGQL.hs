{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Morpheus.Rendering.RenderGQL
  ( RenderGQL(..)
  ) where

import           Data.Semigroup                    ((<>))

-- MORPHEUS
import           Data.Morpheus.Types.Internal.Data

class RenderGQL a where
  render :: a -> Key
  renderWrapped :: a -> [WrapperD] -> Key
  default renderWrapped :: a -> [WrapperD] -> Key
  renderWrapped x wrappers = showGQLWrapper (toGQLWrapper wrappers)
    where
      showGQLWrapper []               = render x
      showGQLWrapper (ListType:xs)    = "[" <> showGQLWrapper xs <> "]"
      showGQLWrapper (NonNullType:xs) = showGQLWrapper xs <> "!"

instance RenderGQL Key where
  render = id

instance RenderGQL TypeAlias where
  render TypeAlias {aliasTyCon, aliasWrappers} = renderWrapped aliasTyCon aliasWrappers

instance RenderGQL DataKind where
  render (ScalarKind x) = typeName x
  render (EnumKind x)   = typeName x
  render (ObjectKind x) = typeName x
  render (UnionKind x)  = typeName x
