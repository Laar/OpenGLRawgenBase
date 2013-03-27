module Language.OpenGLRaw.Interface.Serialize (
    writePackage, writeModule,
    readPackage, readModule,
) where

import System.Directory
import System.FilePath
import Text.XML.Light

import Language.OpenGLRaw.Interface.Types
import Language.OpenGLRaw.Interface.Xml

writePackage :: FilePath -> OpenGLRawI -> IO ()
writePackage path package =
    let path' = packageFile path
        xmlPackage = toGLXml package
    in safeWriteFile path' $ ppTopElement xmlPackage

writeModule :: FilePath -> ModuleI -> IO ()
writeModule path m =
    let path' = moduleFile (modName m) path
        xmlModule = toGLXml m
    in safeWriteFile path' $ ppTopElement xmlModule

readPackage :: FilePath -> IO (Either String OpenGLRawI)
readPackage fp =
    let path = packageFile fp
    in parse `fmap` readFile path

readModule :: FilePath -> ModuleName -> IO (Either String ModuleI)
readModule fp mn =
    let path = moduleFile mn fp
    in parse `fmap` readFile path

packageFile :: FilePath -> FilePath
packageFile fp = fp </> "package" <.> "xml"

moduleFile :: ModuleName -> FilePath -> FilePath
moduleFile mn fp = fp </> "modules" </> moduleNameToPath mn <.> "xml"

parse :: GLXml t => String -> Either String t
parse s = case parseXMLDoc s of
    Nothing -> Left "Xml parsing failed"
    Just r  -> fromGLXml r

-- copy from CodeGenerating
-- | Converts the module name to the path of it's source code.
moduleNameToPath :: ModuleName -> FilePath
moduleNameToPath (ModuleName n) = foldr replace [] n
    where
        replace '.' p = pathSeparator : p
        replace c p = c : p

-- copy from OpenGLRawgen
safeWriteFile :: FilePath -> String -> IO ()
safeWriteFile fp fc = createDirectoryIfMissing True (dropFileName fp)
     >> writeFile fp fc
