module Data.Morpheus.Parsing.Internal.Pattern (
  directive ) where



import           Text.Megaparsec.Char                    (char)
-- MORPHEUS
import           Data.Morpheus.Parsing.Internal.Internal (Parser)
import           Data.Morpheus.Parsing.Internal.Terms    (parseAssignment, parseTuple, qualifier)
import           Data.Morpheus.Parsing.Internal.Value    (parseValue)

-- @directive ( arg1: "value" , .... )
-- TODO:  returns real DataType
directive :: Parser ()
directive = do
    _ <- char '@'
    _name <- qualifier
    _boo <- parseTuple (parseAssignment qualifier parseValue) -- TODO: string
    pure ()
