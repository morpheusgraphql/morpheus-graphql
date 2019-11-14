{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Parsing.Internal.Internal
  ( Parser
  , Position
  , processErrorBundle
  , getLocation
  )
where

import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Morpheus.Error.Utils      ( toLocation )
import           Data.Morpheus.Types.Internal.AST.Base
                                                ( Position )
import           Data.Morpheus.Types.Internal.Validation
                                                ( GQLError(..)
                                                , GQLErrors
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Void                      ( Void )
import           Text.Megaparsec                ( ParseError
                                                , ParseErrorBundle
                                                  ( ParseErrorBundle
                                                  )
                                                , Parsec
                                                , SourcePos
                                                , attachSourcePos
                                                , bundleErrors
                                                , bundlePosState
                                                , errorOffset
                                                , getSourcePos
                                                , parseErrorPretty
                                                )


getLocation :: Parser Position
getLocation = fmap toLocation getSourcePos

type Parser = Parsec Void Text

processErrorBundle :: ParseErrorBundle Text Void -> GQLErrors
processErrorBundle = fmap parseErrorToGQLError . bundleToErrors
 where
  parseErrorToGQLError :: (ParseError Text Void, SourcePos) -> GQLError
  parseErrorToGQLError (err, position) = GQLError
    { desc      = pack (parseErrorPretty err)
    , positions = [toLocation position]
    }
  bundleToErrors
    :: ParseErrorBundle Text Void -> [(ParseError Text Void, SourcePos)]
  bundleToErrors ParseErrorBundle { bundleErrors, bundlePosState } =
    NonEmpty.toList $ fst $ attachSourcePos errorOffset
                                            bundleErrors
                                            bundlePosState
