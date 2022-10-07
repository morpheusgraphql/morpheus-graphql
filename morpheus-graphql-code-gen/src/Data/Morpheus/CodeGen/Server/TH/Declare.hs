{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server.TH.Declare
  ( runDeclare,
  )
where

import Data.Morpheus.CodeGen.Server.Internal.AST
  ( CodeGenConfig (..),
    ServerTypeDefinition,
  )
import Data.Morpheus.CodeGen.Server.TH.GQLDirective
  ( deriveGQLDirective,
  )
import Data.Morpheus.CodeGen.Server.TH.GQLType
  ( deriveGQLType,
  )
import Data.Morpheus.CodeGen.Server.TH.Type
  ( declareType,
  )
import Data.Morpheus.CodeGen.Server.TH.Utils (ServerDec)
import Language.Haskell.TH
import Relude

runDeclare :: Declare a => CodeGenConfig -> a -> Q [Dec]
runDeclare ctx a = runReaderT (declare a) ctx

class Declare a where
  declare :: a -> ServerDec [Dec]

instance Declare a => Declare [a] where
  declare = fmap concat . traverse declare

instance Declare ServerTypeDefinition where
  declare typeDef = do
    let typeDecs = declareType typeDef
    gqlDirDecs <- deriveGQLDirective typeDef
    gqlTypeDecs <- deriveGQLType typeDef
    pure (typeDecs <> gqlDirDecs <> gqlTypeDecs)
