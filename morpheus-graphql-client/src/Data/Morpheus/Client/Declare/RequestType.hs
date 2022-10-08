{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Declare.RequestType
  ( declareRequestType,
  )
where

import Data.Morpheus.Client.Fetch.RequestType
  ( RequestType (..),
  )
import Data.Morpheus.Client.Internal.Types
  ( FetchDefinition (..),
    TypeNameTH (..),
  )
import Data.Morpheus.CodeGen.TH
  ( applyCons,
    funDProxy,
    toCon,
    typeInstanceDec,
  )
import qualified Data.Text as T
import Language.Haskell.TH
  ( Dec,
    Q,
    Type,
    cxt,
    instanceD,
  )
import Relude hiding (ByteString, Type)

declareRequestType :: Text -> FetchDefinition -> Q [Dec]
declareRequestType query FetchDefinition {clientArgumentsTypeName, rootTypeName, fetchOperationType} =
  pure <$> instanceD (cxt []) iHead methods
  where
    queryString = T.unpack query
    typeName = typename rootTypeName
    iHead = applyCons ''RequestType [typeName]
    methods =
      funDProxy
        [ ('__name, [|typeName|]),
          ('__query, [|queryString|]),
          ('__type, [|fetchOperationType|])
        ]
        <> [pure $ typeInstanceDec ''RequestArgs (toCon typeName) (argumentType clientArgumentsTypeName)]

argumentType :: Maybe TypeNameTH -> Type
argumentType Nothing = toCon ("()" :: String)
argumentType (Just clientTypeName) = toCon (typename clientTypeName)
