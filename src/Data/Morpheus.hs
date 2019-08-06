{-# LANGUAGE TemplateHaskell #-}

-- | Build GraphQL APIs with your favourite functional language!
module Data.Morpheus
  ( Interpreter(..)
  , myTestTH
  ) where

import           Data.Morpheus.Interpreter              (Interpreter (..))
import           Data.Text                              (Text (..), pack, unpack)
import           Data.Void                              (Void)
import           Language.Haskell.TH.LanguageExtensions
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Text.Megaparsec
import           Text.Megaparsec.Char

data MyList
  = MyList [Text]
  | Empty

myTestTH :: QuasiQuoter
myTestTH =
  QuasiQuoter
    { quoteExp = compile
    , quotePat = notHandled "patterns"
    , quoteType = notHandled "types"
    , quoteDec = notHandled "declarations"
    }
  where
    notHandled things = error $ things ++ " are not handled by the regex quasiquoter."

compile :: String -> Q Exp
compile inputText =
  case runParser regexParser "<input>" (pack inputText) of
    Left err     -> fail (show err)
    Right regexp -> [|regexp|]

instance Lift Text where
  lift set = appE (varE 'pack) (lift (unpack set))

instance Lift MyList
  -- lift :: RegExp -> Q Exp
                             where
  lift (MyList cs) = apply 'MyList [lift cs]
  lift Empty       = apply 'Empty []

apply :: Name -> [Q Exp] -> Q Exp
apply n = foldl appE (conE n)

parseTuple :: Parsec Void Text a -> Parsec Void Text [a]
parseTuple parser =
  label "Tuple" $
  between (char '(' *> space) (char ')' *> space) (parser `sepBy` (char ',' *> space) <?> "empty Tuple value!")

word :: Parsec Void Text Text
word =
  label "token" $ do
    firstChar <- letterChar <|> char '_'
    restToken <- many $ letterChar <|> char '_' <|> digitChar
    space
    return $ pack $ firstChar : restToken

regexParser :: Parsec Void Text MyList
regexParser = alts <* eof
  where
    alts = MyList <$> parseTuple word
