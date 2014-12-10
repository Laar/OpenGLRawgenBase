module Language.OpenGLRaw.Interface.Serialize (
    writePackage, writeModule,
    readPackage, readModule,
    readPackageModules,
) where

import Control.Applicative
import qualified Data.Map as M

import System.Directory
import System.FilePath
import Text.XML.HXT.Core

import Language.OpenGLRaw.Interface.Types
import Language.OpenGLRaw.Interface.Xml()

writePackage :: FilePath -> OpenGLRawI -> IO ()
writePackage path package =
    let path' = packageFile path
    in writeXmlFile path' package

writeModule :: FilePath -> ModuleI -> IO ()
writeModule path m =
    let path' = moduleFile (modName m) path
    in writeXmlFile path' m

readPackage :: FilePath -> IO (Either String OpenGLRawI)
readPackage fp =
    let path = packageFile fp
    in parseFile path

readModule :: FilePath -> ModuleName -> IO (Either String ModuleI)
readModule fp mn =
    let path = moduleFile mn fp
    in parseFile path

readPackageModules :: FilePath -> IO (Either String (OpenGLRawI, [ModuleI]))
readPackageModules fp = do
    epkg <- readPackage fp
    case epkg of
        Left e -> return $ Left e
        Right pkg -> fmap ((,) pkg) <$> (readModules . M.keys . rawMods $ pkg)
  where
    readModules :: [ModuleName] -> IO (Either String [ModuleI])
    readModules []     = return $ Right []
    readModules (m:ms) = readModule fp m >>= \em -> case em of
            Left e -> return $ Left e
            Right modu -> either (Left) (Right . (modu:)) `fmap` readModules ms

packageFile :: FilePath -> FilePath
packageFile fp = fp </> "package" <.> "xml"

moduleFile :: ModuleName -> FilePath -> FilePath
moduleFile mn fp = fp </> "modules" </> moduleNameToPath mn <.> "xml"

parseFile :: XmlPickler xml => FilePath -> IO (Either String xml)
parseFile fp = do
    results <- runX (
        readDocument readOpts fp 
            >>> removeAllWhiteSpace
            >>> removeAllComment
            >>> arr (unpickleDoc' xpickle)
     )
    return $ handleResults results
  where
    readOpts :: [SysConfig]
    readOpts = [withValidate no, withPreserveComment no]
    handleResults rs = case rs of
        []      -> Left "No parse"
        (_:_:_) -> Left "Multiple parse"
        [rc]    -> rc

writeXmlFile :: XmlPickler xml => FilePath -> xml -> IO ()
writeXmlFile fp xml = do
    createDirectoryIfMissing True (dropFileName fp) 
    _ <- runX (
        constA (pickleDoc xpickle xml) >>> writeDocument writeOpts fp
     )
    return ()
  where
    writeOpts :: [SysConfig]
    writeOpts =
        [ withIndent yes
        ]

-- copy from CodeGenerating
-- | Converts the module name to the path of it's source code.
moduleNameToPath :: ModuleName -> FilePath
moduleNameToPath (ModuleName n) = foldr replace [] n
    where
        replace '.' p = pathSeparator : p
        replace c p = c : p
