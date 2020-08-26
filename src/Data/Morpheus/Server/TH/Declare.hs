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
import Data.Morpheus.Types.Internal.AST
  ( IN,
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
  declare ctx typeD@ServerTypeDefinition {typeArgD, typeOriginal} =
    do
      let mainType = runReader (declareType typeD) ctx
      argTypes <- declareArgTypes ctx typeArgD
      gqlInstances <- deriveGQLInstances
      typeClasses <- deriveGQLType ctx typeD
      introspectEnum <- instanceIntrospect typeOriginal
      pure $ mainType <> typeClasses <> argTypes <> gqlInstances <> introspectEnum
    where
      deriveGQLInstances = concat <$> sequence gqlInstances
        where
          gqlInstances = []

declareArgTypes :: ServerDecContext -> [ServerTypeDefinition IN s] -> Q [Dec]
declareArgTypes ctx types =
  pure $
    concatMap
      (\x -> runReader (declareType x) ctx)
      types
