{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Data.Morpheus.Client
  ( gql
  , defineQuery
  , Fetch(..)
  ) where

--import           Data.Data
import           Data.Aeson                           (encode)
import qualified Data.ByteString.Lazy                 as L (readFile)
import           Data.ByteString.Lazy.Char8           (ByteString, unpack)
import qualified Data.Text                            as T (pack)

--- Template Haskell
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

-- import           Language.Haskell.TH.Lib
-- import           Language.Haskell.TH.Syntax
--
--  Morpheus
import           Data.Morpheus.Client.Build           (defineQuery,Fetch(..))
import           Data.Morpheus.Client.Selection       (operationTypes)
import           Data.Morpheus.Document.ParseDocument (parseFullGQLDocument)
import           Data.Morpheus.Error.Utils            (renderErrors)
import           Data.Morpheus.Parser.Parser          (parseGQL)
import           Data.Morpheus.Types.IO               (GQLRequest (..))
import           Data.Morpheus.Validation.Validation  (validateRequest)

gql :: QuasiQuoter
gql =
  QuasiQuoter
    { quoteExp = compile
    , quotePat = notHandled "patterns"
    , quoteType = notHandled "types"
    , quoteDec = notHandled "declarations"
    }
  where
    notHandled things = error $ things ++ " are not handled by the regex quasiquoter."

readSchema :: IO ByteString
readSchema = L.readFile "./assets/simple.gql"

compile :: String -> Q Exp
compile queryText = do
  eitherSchema <- parseFullGQLDocument <$> runIO readSchema
  case eitherSchema of
    Left errors -> fail (show errors)
    Right schema ->
      case parseGQL request >>= validateRequest schema of
        Left errors -> fail gqlCompileErrors
          where gqlCompileErrors = unpack $ encode $ renderErrors errors
        Right operation -> [|buildCon|]
          where buildCon :: ([(String, [(String, String)])], String)
                buildCon =
                  case operationTypes schema operation of
                    Left err -> fail $ show err
                    Right x  -> (x, queryText)
  where
    request = GQLRequest {query = T.pack queryText, operationName = Nothing, variables = Nothing}
------ LIFT --------------------------------------------
{-
instance (Lift (Operator' a b)) => Lift (Operator a b) where
  lift (Query x)        = apply 'Query [lift x]
  lift (Mutation x)     = apply 'Mutation [lift x]
  lift (Subscription x) = apply 'Subscription [lift x]

instance (Lift a, Lift b) => Lift (Operator' a b) where
  lift (Operator' name args sel pos) = apply 'Operator' [lift name, lift args, lift sel, lift pos]

instance Lift SourcePos where
  lift (SourcePos a b c ) = apply 'SourcePos []

apply :: Name -> [Q Exp] -> Q Exp
apply n = foldl appE (conE n)
-}
