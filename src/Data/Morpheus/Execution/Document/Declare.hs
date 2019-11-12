{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Execution.Document.Declare
  ( declareTypes
  )
where

import           Data.Semigroup                 ( (<>) )
import           Language.Haskell.TH

--
-- MORPHEUS
import           Data.Morpheus.Execution.Document.Decode
                                                ( deriveDecode )
import           Data.Morpheus.Execution.Document.Encode
                                                ( deriveEncode )
import           Data.Morpheus.Execution.Document.GQLType
                                                ( deriveGQLType )
import           Data.Morpheus.Execution.Document.Introspect
                                                ( deriveObjectRep
                                                , instanceIntrospect
                                                )
import           Data.Morpheus.Execution.Internal.Declare
                                                ( declareType )
import           Data.Morpheus.Types.Internal.AST.Data
                                                ( isInput
                                                , isObject
                                                , GQLTypeD(..)
                                                )

declareTypes :: Bool -> [GQLTypeD] -> Q [Dec]
declareTypes namespace = fmap concat . traverse (declareGQLType namespace)

declareGQLType :: Bool -> GQLTypeD -> Q [Dec]
declareGQLType namespace gqlType@GQLTypeD { typeD, typeKindD, typeArgD, typeOriginal }
  = do
    mainType       <- declareMainType
    argTypes       <- declareArgTypes
    gqlInstances   <- deriveGQLInstances
    typeClasses    <- deriveGQLType gqlType
    introspectEnum <- instanceIntrospect typeOriginal
    pure $ mainType <> typeClasses <> argTypes <> gqlInstances <> introspectEnum
 where
  deriveGQLInstances = concat <$> sequence gqlInstances
   where
    gqlInstances
      | isObject typeKindD && isInput typeKindD
      = [deriveObjectRep (typeD, Just typeKindD), deriveDecode typeD]
      | isObject typeKindD
      = [deriveObjectRep (typeD, Just typeKindD), deriveEncode gqlType]
      | otherwise
      = []
  --------------------------------------------------
  declareArgTypes = do
    introspectArgs <- concat <$> traverse deriveArgsRep typeArgD
    decodeArgs     <- concat <$> traverse deriveDecode typeArgD
    return $ argsTypeDecs <> decodeArgs <> introspectArgs
   where
    deriveArgsRep args = deriveObjectRep (args, Nothing)
    ----------------------------------------------------
    argsTypeDecs = map (declareType namespace Nothing []) typeArgD
      --------------------------------------------------
  declareMainType = declareT
   where
    declareT =
      pure [declareType namespace (Just typeKindD) derivingClasses typeD]
    derivingClasses | isInput typeKindD = [''Show]
                    | otherwise         = []
