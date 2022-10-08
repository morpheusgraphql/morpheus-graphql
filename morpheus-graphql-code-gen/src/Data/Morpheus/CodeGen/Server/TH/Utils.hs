{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.CodeGen.Server.TH.Utils
  ( constraintTypeable,
    typeNameStringE,
    withPure,
    mkTypeableConstraints,
    m',
    m_,
    funDProxy,
    ServerDec,
    renderTypeVars,
  )
where

import Data.Morpheus.CodeGen.Server.Internal.AST
  ( CodeGenConfig,
  )
import Data.Morpheus.CodeGen.TH
  ( apply,
    funDSimple,
    vars,
    _',
  )
import Data.Morpheus.Types.Internal.AST
  ( TypeName,
    unpackName,
  )
import Data.Text (unpack)
import qualified Data.Text as T
import Language.Haskell.TH
  ( CxtQ,
    DecQ,
    Exp (..),
    ExpQ,
    Lit (..),
    Name,
    Q,
    Type (..),
    cxt,
    mkName,
  )
import Relude hiding (Type)

type ServerDec = ReaderT CodeGenConfig Q

m_ :: Name
m_ = mkName "m"

m' :: Type
m' = VarT m_

renderTypeVars :: [Text] -> [Name]
renderTypeVars = map (mkName . T.unpack)

funDProxy :: [(Name, ExpQ)] -> [DecQ]
funDProxy = map fun
  where
    fun (name, body) = funDSimple name [_'] body

withPure :: Exp -> Exp
withPure = AppE (VarE 'pure)

typeNameStringE :: TypeName -> Exp
typeNameStringE = LitE . StringL . (unpack . unpackName)

constraintTypeable :: Type -> Q Type
constraintTypeable name = pure $ apply ''Typeable [name]

mkTypeableConstraints :: [Name] -> CxtQ
mkTypeableConstraints = cxt . map constraintTypeable . vars
