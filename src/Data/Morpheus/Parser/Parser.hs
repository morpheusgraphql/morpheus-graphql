module Data.Morpheus.Parser.Parser
  ( parseGQL
  , parseLineBreaks
  ) where

import           Control.Applicative               (many, (<|>))
import           Data.Attoparsec.Text              (Parser, parseOnly)
import           Data.Map                          (fromList, toList)
import           Data.Maybe                        (maybe)
import           Data.Morpheus.Error.Syntax        (syntaxError)
import           Data.Morpheus.Parser.Fragment     (fragment)
import           Data.Morpheus.Parser.Internal     (GQLSyntax (..), endParsing)
import qualified Data.Morpheus.Parser.Mutation     as M
import           Data.Morpheus.Parser.Primitive    (getLines)
import qualified Data.Morpheus.Parser.Query        as Q
import qualified Data.Morpheus.Parser.Subscription as S
import           Data.Morpheus.Types.Error         (Validation)
import           Data.Morpheus.Types.JSType        (JSType (..))
import           Data.Morpheus.Types.Request       (GQLRequest (..))
import           Data.Morpheus.Types.Types         (GQLQueryRoot (..))
import           Data.Text                         (Text, pack)

request :: Parser GQLQueryRoot
request = do
  queryValue <- Q.query <|> M.mutation <|> S.subscription
  fragmentLib <- fromList <$> many fragment
  pure GQLQueryRoot {queryBody = queryValue, fragments = fragmentLib, inputVariables = []}

getVariables :: GQLRequest -> [(Text, JSType)]
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
    Left parseError            -> Left $ syntaxError (pack $ show parseError) 0
