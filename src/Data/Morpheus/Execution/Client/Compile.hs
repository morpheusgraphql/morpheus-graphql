{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Execution.Client.Compile
  ( compileSyntax
  , validateWith
  )
where

import qualified Data.Text                     as T
                                                ( pack )
import           Language.Haskell.TH

--
--  Morpheus


import           Data.Morpheus.Error.Client.Client
                                                ( renderGQLErrors
                                                , gqlWarnings
                                                )

import           Data.Morpheus.Execution.Client.Selection
                                                ( operationTypes )
import           Data.Morpheus.Parsing.Request.Parser
                                                ( parseGQL )
import qualified Data.Morpheus.Types.Internal.AST
                                               as O
                                                ( Operation(..) )
import           Data.Morpheus.Types.IO         ( GQLRequest(..) )

import           Data.Morpheus.Types.Internal.AST
                                                ( GQLQuery(..)
                                                , DataTypeLib
                                                , ClientQuery(..)
                                                , VALIDATION_MODE(..)
                                                )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( Validation
                                                , Result(..)
                                                )
import           Data.Morpheus.Validation.Query.Validation
                                                ( validateRequest )

compileSyntax :: String -> Q Exp
compileSyntax queryText = case parseGQL request of
  Failure errors -> fail (renderGQLErrors errors)
  Success { result, warnings } ->
    gqlWarnings warnings >> [|(result, queryText)|]
 where
  request = GQLRequest { query         = T.pack queryText
                       , operationName = Nothing
                       , variables     = Nothing
                       }

validateWith :: DataTypeLib -> (GQLQuery, String) -> Validation ClientQuery
validateWith schema (rawRequest@GQLQuery { operation }, queryText) = do
  validOperation <- validateRequest schema WITHOUT_VARIABLES rawRequest
  (queryArgsType, queryTypes) <- operationTypes
    schema
    (O.operationArguments operation)
    validOperation
  return ClientQuery { queryText, queryTypes, queryArgsType }
