{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TemplateHaskell    #-}

module Data.Morpheus.Client
  ( graphql
  , GQLData
  ) where

import           Data.Data
import           Data.Text                  (Text, pack, unpack)
import           Data.Void                  (Void)
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           Text.Megaparsec
import           Text.Megaparsec.Char

data GQLData
  = MyList [Text]
  | Empty
  deriving (Data, Typeable, Show)

graphql :: QuasiQuoter
graphql =
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

instance Lift GQLData
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

regexParser :: Parsec Void Text GQLData
regexParser = alts <* eof
  where
    alts = MyList <$> parseTuple word
