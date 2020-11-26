{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Server.Internal.TH.Utils
  ( kindName,
    constraintTypeable,
    typeNameStringE,
    withPure,
    o',
    e',
    mkTypeableConstraints,
  )
where

import Data.Morpheus.Internal.TH
  ( apply,
    toName,
    vars,
  )
import Data.Morpheus.Kind
  ( ENUM,
    INTERFACE,
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

o' :: Type
o' = VarT $ toName ("operation" :: TypeName)

e' :: Type
e' = VarT $ toName ("encodeEvent" :: TypeName)

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
kindName KindEnum = ''ENUM
kindName KindList = ''WRAPPER
kindName KindNonNull = ''WRAPPER
kindName KindInterface = ''INTERFACE
kindName _ = ''TYPE
