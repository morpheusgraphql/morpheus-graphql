module Data.Morpheus.Parser.Parser
  ( parseGQL
  , parseLineBreaks
  ) where

import           Control.Applicative                     (many)
import           Data.Attoparsec.Text                    (Parser, parseOnly)
import           Data.Map                                (fromList, toList)
import           Data.Maybe                              (maybe)
import           Data.Morpheus.Error.Syntax              (syntaxError)
import           Data.Morpheus.Parser.Fragment           (fragment)
import           Data.Morpheus.Parser.Internal           (GQLSyntax (..), endParsing)
import           Data.Morpheus.Parser.Operator           (parseAnonymousQuery, parseOperator)
import           Data.Morpheus.Parser.Primitive          (getLines)
import           Data.Morpheus.Parser.Terms              (parseWhenChar)
import           Data.Morpheus.Types.Internal.Validation (Validation)
import           Data.Morpheus.Types.Internal.Value      (Value (..))
import           Data.Morpheus.Types.Request             (GQLRequest (..))
import           Data.Morpheus.Types.Types               (GQLQueryRoot (..))
import           Data.Text                               (Text, pack)

request :: Parser GQLQueryRoot
request = do
  operator' <- parseWhenChar '{' parseAnonymousQuery parseOperator
  fragmentLib <- fromList <$> many fragment
  pure GQLQueryRoot {queryBody = operator', fragments = fragmentLib, inputVariables = []}

getVariables :: GQLRequest -> [(Text, Value)]
getVariables request' = maybe [] toList (variables request')

parseReq :: GQLRequest -> Either String (GQLSyntax GQLQueryRoot)
parseReq requestBody = parseOnly (request >>= endParsing) $ query requestBody

parseLineBreaks :: GQLRequest -> [Int]
parseLineBreaks requestBody =
  case parseOnly getLines $ query requestBody of
    Right x -> x
    Left _  -> []

parseGQL :: GQLRequest -> Validation GQLQueryRoot
parseGQL requestBody =
  case parseReq requestBody of
    Right (Valid root)         -> Right $ root {inputVariables = getVariables requestBody}
    Right (Invalid text index) -> Left $ syntaxError text index
    Left parseError            -> Left $ syntaxError (pack parseError) 0
