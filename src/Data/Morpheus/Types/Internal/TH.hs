{-# LANGUAGE TemplateHaskell   #-}

module Data.Morpheus.Types.Internal.TH where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Data.Text                  (Text, pack, unpack)


liftText :: Text -> ExpQ
liftText x = appE (varE 'pack) (lift (unpack x))

liftTextTuple :: Lift a => (Text, a) -> ExpQ
liftTextTuple (name, x) = tupE [liftText name, lift x]

liftTextMap :: Lift a => [(Text, a)] -> ExpQ
liftTextMap = listE . map liftTextTuple

apply :: Name -> [Q Exp] -> Q Exp
apply n = foldl appE (conE n)