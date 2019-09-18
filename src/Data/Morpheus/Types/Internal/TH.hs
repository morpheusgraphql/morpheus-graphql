{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Types.Internal.TH where

import           Data.Text                  (Text, pack, unpack)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

liftText :: Text -> ExpQ
liftText x = appE (varE 'pack) (lift (unpack x))

liftTextTuple :: Lift a => (Text, a) -> ExpQ
liftTextTuple (name, x) = tupE [liftText name, lift x]

liftTextMap :: Lift a => [(Text, a)] -> ExpQ
liftTextMap = listE . map liftTextTuple

apply :: Name -> [Q Exp] -> Q Exp
apply n = foldl appE (conE n)

applyT :: Name -> [Q Type] -> Q Type
applyT name = foldl appT (conT name)

headT :: Name -> [String] -> Q Type
headT name li = applyT name (map (conT . mkName) li)
