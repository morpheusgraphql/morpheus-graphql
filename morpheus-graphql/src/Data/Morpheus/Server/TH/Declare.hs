{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.TH.Declare
  ( runDeclare,
  )
where

import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenConfig (..),
    ServerTypeDefinition,
  )
import Data.Morpheus.Server.TH.Declare.GQLType
  ( deriveGQLType,
  )
import Data.Morpheus.Server.TH.Declare.Type
  ( declareType,
  )
import Data.Morpheus.Server.TH.Utils (ServerDec)
import Language.Haskell.TH
import Relude

runDeclare :: Declare a => CodeGenConfig -> a -> Q [Dec]
runDeclare ctx a = runReaderT (declare a) ctx

class Declare a where
  declare :: a -> ServerDec [Dec]

instance Declare a => Declare [a] where
  declare = fmap concat . traverse declare

instance Declare ServerTypeDefinition where
  declare typeDef = (declareType typeDef <>) <$> deriveGQLType typeDef
