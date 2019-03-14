module Data.Morpheus.Parser.Parser
  ( parseGQL
  , parseLineBreaks
  ) where

import           Control.Applicative            (many, (<|>))
import           Data.Attoparsec.Text           (Parser, endOfInput, parseOnly, skipSpace)
import           Data.Map                       (Map, fromList)
import           Data.Maybe                     (fromMaybe)
import           Data.Morpheus.Error.Syntax     (syntaxError)
import           Data.Morpheus.Parser.Fragment  (fragment)
import qualified Data.Morpheus.Parser.Mutation  as M
import           Data.Morpheus.Parser.Primitive (getLines)
import qualified Data.Morpheus.Parser.Query     as Q
import           Data.Morpheus.Types.JSType     (JSType (..))
import           Data.Morpheus.Types.Types      (GQLQueryRoot (..), GQLRequest (..), Validation)
import           Data.Text                      (Text, pack)

request :: Parser GQLQueryRoot
request = do
  queryValue <- Q.query <|> M.mutation
  fragmentLib <- fromList <$> many fragment
  skipSpace
  endOfInput
  pure GQLQueryRoot {queryBody = queryValue, fragments = fragmentLib, inputVariables = fromList []}

getVariables :: GQLRequest -> Map Text JSType
getVariables = fromMaybe (fromList []) . variables

parseReq :: GQLRequest -> Either String GQLQueryRoot
parseReq requestBody = parseOnly request $ query requestBody

parseLineBreaks :: GQLRequest -> [Int]
parseLineBreaks requestBody =
  case parseOnly getLines $ query requestBody of
    Right x -> x
    Left _  -> []

parseGQL :: GQLRequest -> Validation GQLQueryRoot
parseGQL requestBody =
  case parseReq requestBody of
    Right root      -> Right $ root {inputVariables = getVariables requestBody}
    Left parseError -> Left $ syntaxError (pack $ show parseError) 0
