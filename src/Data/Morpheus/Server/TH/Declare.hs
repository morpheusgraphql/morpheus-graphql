{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.TH.Declare
  ( declare,
  )
where

-- MORPHEUS
import Control.Applicative (pure)
import Control.Monad.Reader (runReader)
import Data.Foldable (concat)
import Data.Functor (fmap)
import Data.Morpheus.Server.Internal.TH.Types
  ( ServerDecContext (..),
    ServerTypeDefinition (..),
  )
import Data.Morpheus.Server.TH.Declare.GQLType
  ( deriveGQLType,
  )
import Data.Morpheus.Server.TH.Declare.Type
  ( declareType,
  )
import Data.Morpheus.Server.TH.Transform
import Data.Semigroup ((<>))
import Data.Traversable (traverse)
import Language.Haskell.TH
import Prelude
  ( ($),
    (.),
  )

class Declare a where
  declare :: ServerDecContext -> a -> Q [Dec]

instance Declare a => Declare [a] where
  declare namespace = fmap concat . traverse (declare namespace)

instance Declare (TypeDec s) where
  declare namespace (InputType typeD) = declare namespace typeD
  declare namespace (OutputType typeD) = declare namespace typeD

instance Declare (ServerTypeDefinition cat s) where
  declare ctx typeD@ServerTypeDefinition {typeArgD} =
    do
      typeDef <- declareServerType ctx typeD
      argTypes <- traverse (declareServerType ctx) typeArgD
      pure $ typeDef <> concat argTypes

declareServerType :: ServerDecContext -> ServerTypeDefinition cat s -> Q [Dec]
declareServerType ctx argType = do
  typeClasses <- deriveGQLType ctx argType
  let defs = runReader (declareType argType) ctx
  pure (defs <> typeClasses)
