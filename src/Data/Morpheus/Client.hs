{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Client
  ( gql
  , buildRecord
  ) where

--import           Data.Data
import           Data.Aeson                           (encode)
import qualified Data.ByteString.Lazy                 as L (readFile)
import           Data.ByteString.Lazy.Char8           (ByteString, unpack)
import           Data.Text                            (pack)

--- Template Haskell
-- import           Language.Haskell.TH.Lib
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

-- import           Language.Haskell.TH.Syntax
import           Data.Morpheus.Document.ParseDocument (parseGraphQLDocument)

---  Morpheus
import           Data.Morpheus.Client.Build           (buildRecord)
import           Data.Morpheus.Error.Utils            (renderErrors)
import           Data.Morpheus.Parser.Parser          (parseGQL)
import           Data.Morpheus.Types.IO               (GQLRequest (..))
import           Data.Morpheus.Validation.Validation  (validateRequest)

--import           Data.Morpheus.Types.Internal.AST.Operator (Operator (..), Operator' (..))
--import           Data.Morpheus.Types.Types                 (GQLQueryRoot (..))
{-
data GQLData
  = MyList [Text]
  | Empty
  deriving (Data, Typeable, Show)

instance Lift Text where
  lift set = appE (varE 'pack) (lift (unpack set))

instance Lift GQLData where
  lift (MyList cs) = apply 'MyList [lift cs]
  lift Empty       = apply 'Empty []
-}
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
compile inputText = do
  eitherSchema <- parseGraphQLDocument <$> runIO readSchema
  case eitherSchema of
    Left errors -> fail (show errors)
    Right schema ->
      case parseGQL request >>= validateRequest schema of
        Left errors -> fail gqlCompileErrors
          where gqlCompileErrors = unpack $ encode $ renderErrors errors
        Right root -> [|op|]
          where op = show root
      where request = GQLRequest {query = pack inputText, operationName = Nothing, variables = Nothing}
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
