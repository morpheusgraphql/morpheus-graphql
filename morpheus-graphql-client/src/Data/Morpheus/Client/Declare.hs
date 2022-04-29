module Data.Morpheus.Client.Declare
  ( declareTypesIO,
    declareLocalTypesIO,
    declareGlobalTypesIO,
    declareLocalTypes,
    declareGlobalTypes,
  )
where

import Data.Morpheus.Client.Build
import Data.Morpheus.Client.Internal.Types
  ( ExecutableClientDocument,
    Mode (Local),
    Source,
  )
import Data.Morpheus.Client.Internal.Utils (getSource)
import Data.Morpheus.Client.Schema.Parse (parseSchema)
import Language.Haskell.TH (Dec, Q, runIO)

declareTypesIO :: IO Source -> Mode -> ExecutableClientDocument -> Q [Dec]
declareTypesIO doc mode query = do
  schema <- runIO (parseSchema <$> doc)
  defineQueryTypes mode schema query

declareLocalTypesIO :: IO Source -> ExecutableClientDocument -> Q [Dec]
declareLocalTypesIO doc = declareTypesIO doc Local

declareGlobalTypesIO :: IO Source -> Q [Dec]
declareGlobalTypesIO doc = runIO (parseSchema <$> doc) >>= defineGlobalTypes

-- With PATH

declareLocalTypes :: Q FilePath -> ExecutableClientDocument -> Q [Dec]
declareLocalTypes qPath doc = do
  src <- getSource qPath
  declareTypesIO (pure src) Local doc

declareGlobalTypes :: Q FilePath -> Q [Dec]
declareGlobalTypes qPath = do
  src <- getSource qPath
  declareGlobalTypesIO (pure src)
