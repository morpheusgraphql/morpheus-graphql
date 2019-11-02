{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}


module Data.Morpheus.Types.Internal.TH where

import           Data.Text                  (Text, pack, unpack)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

liftMaybeText :: Maybe Text -> ExpQ
liftMaybeText (Just x) = appE  (conE 'Just) (liftText x)
liftMaybeText Nothing  = conE 'Nothing

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

instanceFunD :: Name -> [String] -> Q Exp -> Q Dec
instanceFunD name args body = funD name [clause (map (varP . mkName) args) (normalB body) []]

instanceHeadMultiT :: Name -> Q Type -> [Q Type] -> Q Type
instanceHeadMultiT className iType li = applyT className (iType : li)

-- "User" -> ["name","id"] -> (User name id)
destructRecord :: String -> [String] -> PatQ
destructRecord conName fields = conP (mkName conName) (map (varP . mkName) fields)

typeInstanceDec :: Name -> Type -> Type -> Dec
#if MIN_VERSION_template_haskell(2,15,0)
-- fix breaking changes   
typeInstanceDec typeFamily arg res = TySynInstD (TySynEqn Nothing (AppT (ConT typeFamily) arg) res)  
#else
--    
typeInstanceDec typeFamily arg res = TySynInstD typeFamily (TySynEqn [arg] res)
#endif
