module Data.Morpheus.Parser.Parser
  ( parseLineBreaks
  , parseRequest
  , parseGQL
  ) where

import           Control.Applicative                     (many)
import           Data.Aeson                              (decode)
import           Data.Attoparsec.Text                    (Parser, endOfInput, parseOnly, skipSpace)
import qualified Data.ByteString.Lazy.Char8              as LB (ByteString)
import           Data.Map                                (fromList, toList)
import           Data.Maybe                              (maybe)
import           Data.Morpheus.Error.Syntax              (syntaxError)
import           Data.Morpheus.Parser.Fragment           (fragment)
import           Data.Morpheus.Parser.Internal           (GQLSyntax (..), catchError, parseLinebreakPositions)
import           Data.Morpheus.Parser.Operator           (parseAnonymousQuery, parseOperator)
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
  skipSpace
  endOfInput
  pure GQLQueryRoot {queryBody = operator', fragments = fragmentLib, inputVariables = []}

getVariables :: GQLRequest -> [(Text, Value)]
getVariables request' = maybe [] toList (variables request')

parseReq :: GQLRequest -> Either String (GQLSyntax GQLQueryRoot)
parseReq requestBody = parseOnly (catchError request) $ query requestBody

parseLineBreaks :: LB.ByteString -> [Int]
parseLineBreaks req =
  case decode req of
    Just x  -> lineBreaks x
    Nothing -> []
  where
    lineBreaks :: GQLRequest -> [Int]
    lineBreaks requestBody =
      case parseOnly parseLinebreakPositions $ query requestBody of
        Right x -> x
        Left _  -> []

parseGQL :: GQLRequest -> Validation GQLQueryRoot
parseGQL requestBody =
  case parseReq requestBody of
    Right (Valid root)         -> Right $ root {inputVariables = getVariables requestBody}
    Right (Invalid text index) -> Left $ syntaxError text index
    Left parseError            -> Left $ syntaxError (pack parseError) 0

parseRequest :: LB.ByteString -> Validation GQLQueryRoot
parseRequest text =
  case decode text of
    Just body -> parseGQL body
    Nothing   -> Left $ syntaxError (pack $ show text) 0
