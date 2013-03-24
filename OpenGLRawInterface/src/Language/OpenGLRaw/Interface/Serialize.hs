module Language.OpenGLRaw.Interface.Serialize (
    writePackage, writeModule,
) where

import System.Directory
import System.FilePath
import Text.XML.Light

import Language.OpenGLRaw.Interface.Types
import Language.OpenGLRaw.Interface.Xml

writePackage :: FilePath -> OpenGLRawI -> IO ()
writePackage path package =
    let path' = path </> "package" <.> "xml"
        xmlPackage = toGLXml package
    in safeWriteFile path' $ ppTopElement xmlPackage

writeModule :: FilePath -> ModuleI -> IO ()
writeModule path m =
    let path' = path </> "modules" </> moduleNameToPath (modName m) <.> "xml"
        xmlModule = toGLXml m
    in safeWriteFile path' $ ppTopElement xmlModule

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
