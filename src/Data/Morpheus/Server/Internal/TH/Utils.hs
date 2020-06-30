{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Server.Internal.TH.Utils
  ( kindName,
    constraintTypeable,
    typeNameStringE,
    withPure,
    o',
    e',
  )
where

import Data.Morpheus.Internal.TH
  ( mkFieldName,
    typeT,
  )
import Data.Morpheus.Kind
  ( ENUM,
    INPUT,
    INTERFACE,
    OUTPUT,
    SCALAR,
    WRAPPER,
  )
import Data.Morpheus.Types.Internal.AST
  ( TypeKind (..),
    TypeName (..),
  )
import Data.Text (unpack)
import Data.Typeable (Typeable)
import Language.Haskell.TH
  ( Exp (..),
    Lit (..),
    Name,
    Q,
    Type (..),
  )

o' :: Type
o' = VarT (mkFieldName "oparation")

e' :: Type
e' = VarT (mkFieldName "encodeEvent")

withPure :: Exp -> Exp
withPure = AppE (VarE 'pure)

typeNameStringE :: TypeName -> Exp
typeNameStringE = LitE . StringL . (unpack . readTypeName)

constraintTypeable :: TypeName -> Q Type
constraintTypeable name = typeT ''Typeable [name]

kindName :: TypeKind -> Name
kindName KindObject {} = ''OUTPUT
kindName KindScalar = ''SCALAR
kindName KindEnum = ''ENUM
kindName KindUnion = ''OUTPUT
kindName KindInputObject = ''INPUT
kindName KindList = ''WRAPPER
kindName KindNonNull = ''WRAPPER
kindName KindInputUnion = ''INPUT
kindName KindInterface = ''INTERFACE
