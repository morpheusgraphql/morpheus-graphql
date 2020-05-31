{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Server.TH.Declare
  ( declare,
  )
where

-- MORPHEUS

import Data.Morpheus.Server.Internal.TH.Types
  ( ServerTypeDefinition (..),
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
    TypeKind (..),
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

instance Declare TypeDec where
  type DeclareCtx TypeDec = Bool
  declare namespace (InputType typeD) = declare namespace typeD
  declare namespace (OutputType typeD) = declare namespace typeD

instance Declare (ServerTypeDefinition cat) where
  type DeclareCtx (ServerTypeDefinition cat) = Bool
  declare namespace typeD@ServerTypeDefinition {tKind, typeArgD, typeOriginal} =
    do
      let mainType = declareMainType (namespace, tKind) typeD
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
              [deriveObjectRep typeD, deriveEncode typeD]
            | otherwise =
              []

declareArgTypes :: Bool -> [ServerTypeDefinition IN] -> Q [Dec]
declareArgTypes namespace types = do
  introspectArgs <- concat <$> traverse deriveObjectRep types
  decodeArgs <- concat <$> traverse deriveDecode types
  return $ argsTypeDecs <> decodeArgs <> introspectArgs
  where
    ----------------------------------------------------
    argsTypeDecs = map (declareType namespace Nothing []) types

declareMainType :: (Bool, TypeKind) -> ServerTypeDefinition cat -> [Dec]
declareMainType (_, KindScalar) _ = []
declareMainType (namespace, tKind) typeD =
  [declareType namespace (Just tKind) derivingClasses typeD]
  where
    -- input types support show
    derivingClasses
      | isInput tKind = [''Show]
      | otherwise = []
