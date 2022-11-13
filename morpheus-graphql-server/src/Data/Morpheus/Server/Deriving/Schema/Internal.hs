{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Deriving.Schema.Internal
  ( CatType (..),
    TyContentM,
    fromSchema,
  )
where

-- MORPHEUS

import Data.Morpheus.Internal.Ext
  ( GQLResult,
    Result (Failure, Success, errors),
  )
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( CatType (..),
  )
import Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
  )
import Data.Morpheus.Types.Internal.AST
  ( ArgumentsDefinition,
    CONST,
    Schema (..),
    VALID,
  )
import Language.Haskell.TH (Exp, Q)
import Relude hiding (empty)

fromSchema :: GQLResult (Schema VALID) -> Q Exp
fromSchema Success {} = [|()|]
fromSchema Failure {errors} = fail (show errors)

type TyContentM kind = SchemaT kind (Maybe (ArgumentsDefinition CONST))
