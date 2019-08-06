{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Morpheus.Client
  ( gql
  ) where

--import           Data.Data
import           Data.Morpheus.Parser.Parser (parseGQLSyntax)

import           Data.Text                   (pack)

--- Template Haskell
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax

-- import           Text.Megaparsec                           (SourcePos (..))
---  Morpheus
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

compile :: String -> Q Exp
compile inputText =
  case parseGQLSyntax (pack inputText) of
    Left err -> fail (show err)
    Right root -> [|op|]
      where op = show root

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

-}
apply :: Name -> [Q Exp] -> Q Exp
apply n = foldl appE (conE n)
