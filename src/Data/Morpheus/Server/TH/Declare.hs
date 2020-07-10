{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Server.TH.Declare
  ( declare,
  )
where

-- MORPHEUS

import Data.Morpheus.Server.Internal.TH.Types
  ( ServerTypeDefinition (..),
  )
import Data.Morpheus.Server.TH.Declare.Channels
  ( deriveChannels,
  )
import Data.Morpheus.Server.TH.Declare.Decode
  ( deriveDecode,
  )
import Data.Morpheus.Server.TH.Declare.Encode
  ( deriveEncode,
  )
import Data.Morpheus.Server.TH.Declare.GQLType
  ( deriveGQLType,
  )
import Data.Morpheus.Server.TH.Declare.Introspect
  ( deriveObjectRep,
    instanceIntrospect,
  )
import Data.Morpheus.Server.TH.Declare.Type
  ( declareType,
  )
import Data.Morpheus.Server.TH.Transform
import Data.Morpheus.Types.Internal.AST
  ( IN,
    isInput,
    isObject,
  )
import Data.Semigroup ((<>))
import Language.Haskell.TH

class Declare a where
  type DeclareCtx a :: *
  declare :: DeclareCtx a -> a -> Q [Dec]

instance Declare a => Declare [a] where
  type DeclareCtx [a] = DeclareCtx a
  declare namespace = fmap concat . traverse (declare namespace)

instance Declare (TypeDec s) where
  type DeclareCtx (TypeDec s) = Bool
  declare namespace (InputType typeD) = declare namespace typeD
  declare namespace (OutputType typeD) = declare namespace typeD

instance Declare (ServerTypeDefinition cat s) where
  type DeclareCtx (ServerTypeDefinition cat s) = Bool
  declare namespace typeD@ServerTypeDefinition {tKind, typeArgD, typeOriginal} =
    do
      let mainType = declareType namespace typeD
      argTypes <- declareArgTypes namespace typeArgD
      gqlInstances <- deriveGQLInstances
      typeClasses <- deriveGQLType typeD
      introspectEnum <- instanceIntrospect typeOriginal
      pure $ mainType <> typeClasses <> argTypes <> gqlInstances <> introspectEnum
    where
      deriveGQLInstances = concat <$> sequence gqlInstances
        where
          gqlInstances
            | isObject tKind && isInput tKind =
              [deriveObjectRep typeD, deriveDecode typeD]
            | isObject tKind =
              [deriveObjectRep typeD, deriveEncode typeD, deriveChannels typeD]
            | otherwise =
              []

declareArgTypes :: Bool -> [ServerTypeDefinition IN s] -> Q [Dec]
declareArgTypes namespace types = do
  introspectArgs <- concat <$> traverse deriveObjectRep types
  decodeArgs <- concat <$> traverse deriveDecode types
  return $ argsTypeDecs <> decodeArgs <> introspectArgs
  where
    ----------------------------------------------------
    argsTypeDecs = concatMap (declareType namespace) types
