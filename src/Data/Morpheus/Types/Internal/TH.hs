{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}


module Data.Morpheus.Types.Internal.TH where

import           Language.Haskell.TH
import           Data.Text   (Text,unpack)

apply :: Name -> [Q Exp] -> Q Exp
apply n = foldl appE (conE n)

applyT :: Name -> [Q Type] -> Q Type
applyT name = foldl appT (conT name)

typeT :: Name -> [Text] -> Q Type
typeT name li = applyT name (map (varT . mkName . unpack) li)

instanceHeadT :: Name -> Text -> [Text] -> Q Type
instanceHeadT cName iType tArgs = applyT cName [applyT (mkName $ unpack iType) (map (varT . mkName . unpack) tArgs)]

instanceProxyFunD :: (Name,ExpQ) -> DecQ
instanceProxyFunD (name,body)  = instanceFunD name ["_"] body

instanceFunD :: Name -> [Text] -> ExpQ -> Q Dec
instanceFunD name args body = funD name [clause (map (varP . mkName. unpack) args) (normalB body) []]

instanceHeadMultiT :: Name -> Q Type -> [Q Type] -> Q Type
instanceHeadMultiT className iType li = applyT className (iType : li)


-- "User" -> ["name","id"] -> (User name id)
destructRecord :: Text -> [Text] -> PatQ
destructRecord conName fields = conP (mkName $ unpack conName) (map (varP . mkName .unpack) fields)

typeInstanceDec :: Name -> Type -> Type -> Dec
#if MIN_VERSION_template_haskell(2,15,0)
-- fix breaking changes   
typeInstanceDec typeFamily arg res = TySynInstD (TySynEqn Nothing (AppT (ConT typeFamily) arg) res)  
#else
--    
typeInstanceDec typeFamily arg res = TySynInstD typeFamily (TySynEqn [arg] res)
#endif


infoTyVars :: Info -> [TyVarBndr]
infoTyVars (TyConI x) =  decArgs x
infoTyVars _ = []
     
decArgs :: Dec -> [TyVarBndr]
decArgs (DataD _ _ args _ _ _) = args 
decArgs (NewtypeD _ _ args _ _ _) = args 
decArgs (TySynD _ args _) = args
decArgs _ = []
