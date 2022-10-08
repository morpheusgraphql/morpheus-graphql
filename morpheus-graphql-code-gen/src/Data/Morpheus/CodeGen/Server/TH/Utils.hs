{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server.TH.Utils
  ( mkTypeableConstraints,
    m',
    m_,
    renderTypeVars,
  )
where

import Data.Morpheus.CodeGen.TH
  ( apply,
    vars,
  )
import qualified Data.Text as T
import Language.Haskell.TH
  ( CxtQ,
    Name,
    Q,
    Type (..),
    cxt,
    mkName,
  )
import Relude hiding (Type)

m_ :: Name
m_ = mkName "m"

m' :: Type
m' = VarT m_

renderTypeVars :: [Text] -> [Name]
renderTypeVars = map (mkName . T.unpack)

constraintTypeable :: Type -> Q Type
constraintTypeable name = pure $ apply ''Typeable [name]

mkTypeableConstraints :: [Name] -> CxtQ
mkTypeableConstraints = cxt . map constraintTypeable . vars
