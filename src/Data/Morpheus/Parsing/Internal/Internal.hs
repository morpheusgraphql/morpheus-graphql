{-# LANGUAGE NamedFieldPuns #-}

module Data.Morpheus.Parsing.Internal.Internal
  ( Parser
  , Position
  , processErrorBundle
  , getLocation
  , processParser
  )
where

-- import            Data.Set  (toList)
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Morpheus.Error.Utils      ( toLocation )
import           Data.Morpheus.Types.Internal.AST
                                                ( Position )
import           Data.Morpheus.Types.Internal.Resolving
                                                ( GQLError(..)
                                                , GQLErrors
                                                , Validation
                                                , failure
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
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
                                                , runParser
                                                )
-- import          Text.Megaparsec.Error (ParseError(..) , ErrorFancy(..))
import           Data.Void                      ( Void )

getLocation :: Parser Position
getLocation = fmap toLocation getSourcePos

type MyError = Void
type Parser = Parsec MyError Text
type ErrorBundle = ParseErrorBundle Text MyError

processParser :: Parser a -> Text -> Validation a
processParser parser txt = case runParser parser [] txt of
    Right root       -> pure root
    Left  parseError -> failure (processErrorBundle parseError)


processErrorBundle :: ErrorBundle -> GQLErrors
processErrorBundle = fmap parseErrorToGQLError . bundleToErrors
 where
  parseErrorToGQLError :: (ParseError Text MyError, SourcePos) -> GQLError
  parseErrorToGQLError (err, position) = GQLError
    { message   = pack (parseErrorPretty err)
    , locations = [toLocation position]
    }
  bundleToErrors
    :: ErrorBundle -> [(ParseError Text MyError, SourcePos)]
  bundleToErrors ParseErrorBundle { bundleErrors, bundlePosState } =
    NonEmpty.toList $ fst $ attachSourcePos errorOffset
                                            bundleErrors
                                            bundlePosState

-- processErrorBundle :: ErrorBundle -> GQLErrors
-- processErrorBundle = concatMap parseErrorToGQLError . bundleToErrors
--  where
--   parseErrorToGQLError :: (ParseError Text GQLErrors, SourcePos) -> GQLErrors
--   parseErrorToGQLError (FancyError _ ls,_pos) = concatMap bla (toList ls)
--     where 
--         bla (ErrorCustom e) = e
--   parseErrorToGQLError (err, position) =
--     -- [GQLError
--     --   { message   = pack (parseErrorPretty err)
--     --     , locations = [toLocation position]
--     --   }
--     -- ]
--   bundleToErrors
--     :: ErrorBundle -> [(ParseError Text GQLErrors, SourcePos)]
--   bundleToErrors ParseErrorBundle { bundleErrors, bundlePosState } =
--     NonEmpty.toList $ fst $ attachSourcePos errorOffset
--                                             bundleErrors
--                                             bundlePosState
