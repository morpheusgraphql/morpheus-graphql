{-# LANGUAGE TemplateHaskell #-}

module Data.Morpheus.Client.Build
  ( define
  , defineQuery
  , Fetch(..)
  ) where

import           Language.Haskell.TH
import Data.Morpheus.Client.Utils (simpleName,nonCapital)

defineQuery :: ([(String, [(String, String)])], String) -> Q [Dec]
defineQuery (x, queryText) = concat <$> mapM (define queryText) x

class Fetch a where
  queryFor :: a -> String

fetchInstance :: (Name, [Constructor]) -> String -> Q [Dec]
fetchInstance (type_name, constructors) queryText = pure <$> genInstance ''Fetch (conT type_name) constructors methods
  where
    methods = [(mkName "queryFor", gen_method)]
    ---------------------------------------------
    gen_method (_conName, _components) _vars = [|queryText|]

genInstance :: Name -> TypeQ -> [Constructor] -> Funcs -> DecQ
genInstance className forType constructors functions =
  instanceD (cxt []) (appT (conT className) forType) (map defineFunc functions)
  where
    defineFunc (name, funcBody) = funD name (map (genClause funcBody) constructors)
    -------------------------------------------------------------------------------
    genClause :: (Constructor -> [ExpQ] -> ExpQ) -> Constructor -> ClauseQ
    genClause func_body dataCon@(con_name, components) = do
      vars <- mapM var components
      clause [conP con_name (map varP vars)] (normalB (func_body dataCon (map varE vars))) []
      where
        var (_, typ) =
          newName $
          case typ of
            (ConT name) -> nonCapital $ nameBase name
            _   -> "parm"
          where

type Constructor = (Name, [(Maybe Name, Type)])

type Cons_vars = [ExpQ] -- A list of variables that bind in the constructor

type Function_body = ExpQ

type Gen_func = Constructor -> Cons_vars -> Function_body

type Func_name = Name

type Funcs = [(Func_name, Gen_func)]

recDefinition :: (String, [(String, String)]) -> (Name, [Con])
recDefinition (strName, fields) = (typeName, [recordCon])
  where
    typeName = mkName strName
    defBang = Bang NoSourceUnpackedness NoSourceStrictness
    recordCon = RecC typeName (map genField fields)
      where
        genField (fieldName, fType) = (mkName $ _name fieldName, defBang, ConT $ mkName fType)
          where
            _name name = "_" <> name


define :: String -> (String, [(String, String)]) -> Q [Dec]
define query (strName, fields) = do
  instDec <- fetchInstance (typeName, map termA cons) query
  pure (buildRec : instDec)
  where
    (typeName, cons) = recDefinition (strName, fields)
    -----------------------------------------------
    buildRec = DataD [] typeName [] Nothing cons []
    ------------------------------------------------
    termA (NormalC c xs)   = (c, map (\x -> (Nothing, snd x)) xs)
    termA (RecC c xs)      = (c, map (\(n, _, t) -> (Just $ simpleName n, t)) xs)
    termA (InfixC t1 c t2) = (c, [(Nothing, snd t1), (Nothing, snd t2)])


