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
import Data.Morpheus.Server.TH.Declare.GQLType
  ( deriveGQLType,
  )
import Data.Morpheus.Server.TH.Declare.Introspect
  ( instanceIntrospect,
  )
import Data.Morpheus.Server.TH.Declare.Type
  ( declareType,
  )
import Data.Morpheus.Server.TH.Transform
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
  declare ctx typeD@ServerTypeDefinition {typeArgD, typeOriginal} =
    do
      typeDef <- declareServerType ctx typeD
      argTypes <- traverse (declareServerType ctx) typeArgD
      introspectEnum <- instanceIntrospect typeOriginal
      pure $ typeDef <> concat argTypes <> introspectEnum

declareServerType :: ServerDecContext -> ServerTypeDefinition cat s -> Q [Dec]
declareServerType ctx argType = do
  typeClasses <- deriveGQLType ctx argType
  let defs = runReader (declareType argType) ctx
  pure (defs <> typeClasses)
