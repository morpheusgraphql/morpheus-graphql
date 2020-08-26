{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Morpheus.Server.TH.Declare
  ( declare,
  )
where

-- MORPHEUS

import Control.Monad.Reader (runReader)
import Data.Morpheus.Server.Internal.TH.Types
  ( ServerDecContext (..),
    ServerTypeDefinition (..),
  )
import Data.Morpheus.Server.TH.Declare.Channels
  ( deriveChannels,
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
  declare :: ServerDecContext -> a -> Q [Dec]

instance Declare a => Declare [a] where
  declare namespace = fmap concat . traverse (declare namespace)

instance Declare (TypeDec s) where
  declare namespace (InputType typeD) = declare namespace typeD
  declare namespace (OutputType typeD) = declare namespace typeD

instance Declare (ServerTypeDefinition cat s) where
  declare ctx typeD@ServerTypeDefinition {tKind, typeArgD, typeOriginal} =
    do
      let mainType = runReader (declareType typeD) ctx
      argTypes <- declareArgTypes ctx typeArgD
      gqlInstances <- deriveGQLInstances
      typeClasses <- deriveGQLType (namespace ctx) typeD
      introspectEnum <- instanceIntrospect typeOriginal
      pure $ mainType <> typeClasses <> argTypes <> gqlInstances <> introspectEnum
    where
      deriveGQLInstances = concat <$> sequence gqlInstances
        where
          gqlInstances
            | isObject tKind && isInput tKind =
              [deriveObjectRep typeD]
            | isObject tKind =
              [deriveObjectRep typeD, deriveChannels typeD]
            | otherwise =
              []

declareArgTypes :: ServerDecContext -> [ServerTypeDefinition IN s] -> Q [Dec]
declareArgTypes ctx types = do
  introspectArgs <- concat <$> traverse deriveObjectRep types
  return $ argsTypeDecs <> introspectArgs
  where
    ----------------------------------------------------
    argsTypeDecs =
      concatMap
        (\x -> runReader (declareType x) ctx)
        types
