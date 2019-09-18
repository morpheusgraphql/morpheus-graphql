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

typeT :: Name -> [String] -> Q Type
typeT name li = applyT name (map (varT . mkName) li)

instanceHeadT :: Name -> String -> [String] -> Q Type
instanceHeadT cName iType tArgs = applyT cName [applyT (mkName iType) (map (varT . mkName) tArgs)]
--
-- instanceHeadMultiT :: Name -> Q Type -> [String] -> Q Type
-- instanceHeadMultiT className iType li = applyT className (iType : map (varT . mkName) li)
