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
  ( KindedType (..),
    TyContentM,
    TyContent,
    fromSchema,
    lookupDescription,
    lookupDirectives,
    lookupFieldContent,
  )
where

-- MORPHEUS

import qualified Data.Map as M
import Data.Morpheus.Internal.Ext
  ( GQLResult,
    Result (Failure, Success, errors),
  )
import Data.Morpheus.Internal.Utils (empty)
import Data.Morpheus.Server.Deriving.Utils.Kinded
  ( KindedType (..),
  )
import Data.Morpheus.Server.Types.GQLType
  ( GQLType (..),
  )
import Data.Morpheus.Server.Types.SchemaT
  ( SchemaT,
  )
import Data.Morpheus.Types.Internal.AST
  ( CONST,
    Description,
    Directives,
    FieldContent (..),
    Schema (..),
    TRUE,
    VALID,
  )
import Language.Haskell.TH (Exp, Q)
import Relude hiding (empty)

lookupDescription :: GQLType a => f a -> Text -> Maybe Description
lookupDescription proxy name = name `M.lookup` getDescriptions proxy

lookupDirectives :: GQLType a => f a -> Text -> Directives CONST
lookupDirectives proxy name = fromMaybe empty $ name `M.lookup` getDirectives proxy

lookupFieldContent ::
  GQLType a =>
  KindedType kind a ->
  Text ->
  Maybe (FieldContent TRUE kind CONST)
lookupFieldContent proxy@InputType key = DefaultInputValue <$> key `M.lookup` defaultValues proxy
lookupFieldContent OutputType _ = Nothing

fromSchema :: GQLResult (Schema VALID) -> Q Exp
fromSchema Success {} = [|()|]
fromSchema Failure {errors} = fail (show errors)

type TyContentM kind = SchemaT kind (TyContent kind)

type TyContent kind = Maybe (FieldContent TRUE kind CONST)
