{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CLI.Generator
  ( processServerDocument,
    processClientDocument,
    BuildConfig (..),
  )
where

import CLI.Config (ServiceOptions (..))
import Data.ByteString.Lazy.Char8 (ByteString, pack)
import Data.HashMap.Lazy (lookup)
import Data.Morpheus.Client
  ( SchemaSource,
    parseClientTypeDeclarations,
  )
import Data.Morpheus.Client.CodeGen.AST (ClientDeclaration (..), DERIVING_MODE (SCALAR_MODE))
import Data.Morpheus.CodeGen
  ( CodeGenConfig (..),
    parseServerTypeDefinitions,
  )
import Data.Morpheus.CodeGen.Internal.AST
import Data.Morpheus.CodeGen.Utils (Flag (..))
import Data.Morpheus.Internal.Ext (GQLResult)
import Data.Morpheus.Types.Internal.AST (unpackName)
import qualified Data.Set as S
import Prettyprinter
import Relude hiding (ByteString, print)

data BuildConfig = BuildConfig
  { root :: String,
    buildOptions :: ServiceOptions
  }
  deriving (Show)

getExtensions :: [Flag] -> [Text]
getExtensions xs = [x | FlagLanguageExtension x <- xs]

resolveExternal :: ServiceOptions -> Text -> Maybe (Text, [Text])
resolveExternal ServiceOptions {optionExternals} name = (,[name]) <$> name `lookup` optionExternals

collectExternals :: ServiceOptions -> [Text] -> [(Text, [Text])]
collectExternals buildOptions exts = mapMaybe (resolveExternal buildOptions) (S.toList $ S.fromList exts)

getImports :: ServiceOptions -> [Flag] -> [(Text, [Text])]
getImports buildOptions flags = collectExternals buildOptions [x | FlagExternal x <- flags]

uniq :: (Ord a) => [a] -> [a]
uniq = S.toList . S.fromList

processServerDocument :: BuildConfig -> Text -> ByteString -> GQLResult ByteString
processServerDocument BuildConfig {..} moduleName schema = do
  (types, flags) <- second uniq <$> parseServerTypeDefinitions CodeGenConfig {namespace = optionNamespace buildOptions} schema
  pure
    $ print
    $ ModuleDefinition
      { moduleName,
        imports =
          [ ("Data.Morpheus.Server.CodeGen.Internal", ["*"]),
            ("Data.Morpheus.Server.Types", ["*"])
          ]
            <> map (,["*"]) (optionImports buildOptions)
            <> getImports buildOptions flags,
        extensions =
          [ "DeriveGeneric",
            "DuplicateRecordFields",
            "TypeFamilies"
          ]
            <> getExtensions flags,
        types
      }

isScalars :: ClientDeclaration -> Bool
isScalars (InstanceDeclaration SCALAR_MODE _) = True
isScalars _ = False

processClientDocument ::
  BuildConfig ->
  SchemaSource ->
  Maybe Text ->
  Text ->
  GQLResult (Maybe ByteString)
processClientDocument BuildConfig {..} schema query moduleName = do
  (allTypes, flags) <- second uniq <$> parseClientTypeDeclarations schema query
  let externalImports = collectExternals buildOptions [unpackName $ getFullName (typeClassTarget x) | InstanceDeclaration SCALAR_MODE x <- allTypes]
  let types = filter (not . isScalars) allTypes
  if null types
    then pure Nothing
    else
      pure
        $ Just
        $ print
          ModuleDefinition
            { moduleName,
              imports =
                [("Data.Morpheus.Client.CodeGen.Internal", ["*"])]
                  <> map (,["*"]) (optionImports buildOptions)
                  <> externalImports
                  <> getImports buildOptions flags,
              extensions =
                [ "DeriveGeneric",
                  "DuplicateRecordFields",
                  "OverloadedStrings",
                  "TypeFamilies"
                ]
                  <> getExtensions flags,
              types
            }

print :: (Pretty a) => a -> ByteString
print = pack . show . pretty
