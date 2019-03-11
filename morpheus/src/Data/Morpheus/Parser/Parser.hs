module Data.Morpheus.Parser.Parser
  ( parseGQL
  , parseLineBreaks
  ) where

import           Control.Applicative            (many, (<|>))
import           Data.Attoparsec.Text           (Parser, endOfInput, parseOnly,
                                                 skipSpace)
import           Data.Map                       (fromList)
import           Data.Maybe                     (fromMaybe)
import           Data.Morpheus.ErrorMessage     (syntaxError)
import           Data.Morpheus.Parser.Fragment  (fragment)
import qualified Data.Morpheus.Parser.Mutation  as M
import           Data.Morpheus.Parser.Primitive (getLines)
import qualified Data.Morpheus.Parser.Query     as Q
import           Data.Morpheus.Types.Types      (GQLQueryRoot (..),
                                                 GQLRequest (..), Validation)
import           Data.Text                      (pack)

request :: Parser GQLQueryRoot
request = do
  queryValue <- Q.query <|> M.mutation
  fragmentLib <- fromList <$> many fragment
  skipSpace
  endOfInput
  pure GQLQueryRoot {queryBody = queryValue, fragments = fragmentLib, inputVariables = fromList []}

getVariables = fromMaybe (fromList []) . variables

parseReq requestBody = parseOnly request $ query requestBody

parseLineBreaks :: GQLRequest -> [Int]
parseLineBreaks requestBody =
  case parseOnly getLines $ query requestBody of
    Right x -> x
    Left _  -> []

parseGQL :: GQLRequest -> Validation GQLQueryRoot
parseGQL requestBody =
  case parseReq requestBody of
    Right root -> Right $ root {inputVariables = getVariables requestBody}
    Left error -> Left $ syntaxError (pack $ show error) 0
