{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Morpheus.Client.Declare.Fetch
  ( declareFetch,
  )
where

import Data.Morpheus.Client.Fetch
  ( RequestType (..),
  )
import Data.Morpheus.Client.Internal.Types
  ( FetchDefinition (..),
    HTTP,
    METHOD (..),
    TypeNameTH (..),
    WS,
  )
import Data.Morpheus.CodeGen.Internal.TH
  ( applyCons,
    funDSimple,
    toCon,
    typeInstanceDec,
    _',
  )
import qualified Data.Text as T
import Language.Haskell.TH
  ( Dec,
    Name,
    Q,
    Type,
    cxt,
    instanceD,
  )
import Relude hiding (ByteString, Type)

declareFetch :: Text -> FetchDefinition -> Q [Dec]
declareFetch query FetchDefinition {clientArgumentsTypeName, rootTypeName, fetchMethod} =
  pure <$> instanceD (cxt []) iHead methods
  where
    queryString = T.unpack query
    typeName = typename rootTypeName
    iHead = applyCons ''RequestType [typeName]
    methods =
      [ funDSimple '__name [_'] [|typeName|],
        funDSimple '__query [_'] [|queryString|],
        pure $ typeInstanceDec ''RequestArgs (toCon typeName) (argumentType clientArgumentsTypeName),
        pure $ typeInstanceDec ''RequestMethod (toCon typeName) (toCon $ getMethod fetchMethod)
      ]

getMethod :: METHOD -> Name
getMethod Ws = ''WS
getMethod Http = ''HTTP

argumentType :: Maybe TypeNameTH -> Type
argumentType Nothing = toCon ("()" :: String)
argumentType (Just clientTypeName) = toCon (typename clientTypeName)
