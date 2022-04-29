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

import Data.Morpheus.Client.Fetch (Fetch (..))
import Data.Morpheus.Client.Internal.Types
  ( FetchDefinition (..),
    TypeNameTH (..),
  )
import Data.Morpheus.CodeGen.Internal.TH
  ( applyCons,
    toCon,
    typeInstanceDec,
  )
import qualified Data.Text as T
import Language.Haskell.TH
  ( Dec,
    Q,
    Type,
    clause,
    cxt,
    funD,
    instanceD,
    normalB,
  )
import Relude hiding (ByteString, Type)

declareFetch :: Text -> FetchDefinition -> Q [Dec]
declareFetch query FetchDefinition {clientArgumentsTypeName, rootTypeName} =
  pure <$> instanceD (cxt []) iHead methods
  where
    queryString = T.unpack query
    typeName = typename rootTypeName
    iHead = applyCons ''Fetch [typeName]
    methods =
      [ funD 'fetch [clause [] (normalB [|__fetch queryString typeName|]) []],
        pure $ typeInstanceDec ''Args (toCon typeName) (argumentType clientArgumentsTypeName)
      ]

argumentType :: Maybe TypeNameTH -> Type
argumentType Nothing = toCon ("()" :: String)
argumentType (Just clientTypeName) = toCon (typename clientTypeName)
