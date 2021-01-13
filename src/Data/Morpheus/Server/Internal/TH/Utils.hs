{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Internal.TH.Utils
  ( kindName,
    constraintTypeable,
    typeNameStringE,
    withPure,
    mkTypeableConstraints,
  )
where

import Data.Morpheus.Internal.TH
  ( apply,
    vars,
  )
import Data.Morpheus.Kind
  ( INTERFACE,
    SCALAR,
    TYPE,
    WRAPPER,
  )
import Data.Morpheus.Types.Internal.AST
  ( TypeKind (..),
    TypeName (..),
  )
import Data.Text (unpack)
import Data.Typeable (Typeable)
import Language.Haskell.TH
  ( CxtQ,
    Exp (..),
    Lit (..),
    Name,
    Type (..),
    cxt,
  )
import Prelude
  ( ($),
    (.),
    String,
    map,
    pure,
  )

withPure :: Exp -> Exp
withPure = AppE (VarE 'pure)

typeNameStringE :: TypeName -> Exp
typeNameStringE = LitE . StringL . (unpack . readTypeName)

constraintTypeable :: Type -> Type
constraintTypeable name = apply ''Typeable [name]

mkTypeableConstraints :: [String] -> CxtQ
mkTypeableConstraints args = cxt $ map (pure . constraintTypeable) (vars args)

kindName :: TypeKind -> Name
kindName KindScalar = ''SCALAR
kindName KindList = ''WRAPPER
kindName KindNonNull = ''WRAPPER
kindName KindInterface = ''INTERFACE
kindName _ = ''TYPE
