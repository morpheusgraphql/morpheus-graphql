{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.TH.Utils
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

import Data.Morpheus.CodeGen.Internal.AST
  ( CodeGenConfig,
  )
import Data.Morpheus.Internal.TH
  ( _',
    apply,
    funDSimple,
    vars,
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

renderTypeVars :: [TypeName] -> [Name]
renderTypeVars = map (mkName . T.unpack . unpackName)

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
