{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Server.Internal.TH.Utils
  ( kindName,
    constraintTypeable,
    typeNameStringE,
    fieldNameStringE,
    mkFieldsE,
    withPure,
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
  ( FieldDefinition (..),
    FieldName (..),
    TypeKind (..),
    TypeName (..),
  )
import Data.Text (unpack)
import Data.Typeable (Typeable)
import Language.Haskell.TH
  ( Exp (..),
    Lit (..),
    Name,
    Q,
    Type,
  )

withPure :: Exp -> Exp
withPure = AppE (VarE 'pure)

--  input : mapFields 'mkValue [FieldDefinition { fieldName = "field1",..} ,..]
--  expression: [("field1" , mkValue field1), ..]
mkFieldsE :: Name -> [FieldDefinition cat] -> [Exp]
mkFieldsE name = map (mkEntryWith name)

--  input : mkFieldWith 'mkValue (FieldDefinition { fieldName = "field1", ..})
--  expression: ("field1" ,mkValue field1)
mkEntryWith ::
  Name ->
  FieldDefinition cat ->
  Exp
mkEntryWith f FieldDefinition {fieldName} =
  TupE
    [ Just (fieldNameStringE fieldName),
      Just (AppE (VarE f) (varNameE fieldName))
    ]

varNameE :: FieldName -> Exp
varNameE = VarE . mkFieldName

typeNameStringE :: TypeName -> Exp
typeNameStringE = LitE . StringL . (unpack . readTypeName)

fieldNameStringE :: FieldName -> Exp
fieldNameStringE (FieldName x) = LitE $ StringL (unpack x)

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
