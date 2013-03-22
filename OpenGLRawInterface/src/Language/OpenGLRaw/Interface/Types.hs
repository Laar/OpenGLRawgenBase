module Language.OpenGLRaw.Interface.Types (
    OpenGLRawI(..),
    ModuleI(..), ModuleType(..),
    Major, Minor, Deprecated,
    FuncI(..), EnumI(..), Reexport(..),

    -- * Convenience reexport
    ModuleName(..),
) where

import qualified Data.Set as S

import Language.Haskell.Exts.Syntax(ModuleName(..))
import Language.OpenGLRaw.Base

type HsName = String

data OpenGLRawI
    = OpenGLRawI
    { rawMods :: S.Set ModuleName
    } deriving (Show)

data ModuleI
    = ModuleI
    { modName  :: ModuleName
    , modType  :: ModuleType
    , modEnums :: S.Set EnumI
    , modFuncs :: S.Set FuncI
    , modReExports :: S.Set Reexport
    } deriving (Show)

type Major = Int
type Minor = Int
type Deprecated = Bool

data ModuleType
    = CoreInterface Major Minor Deprecated
    | ExtensionMod  Extension String Deprecated
    | TopLevelGroup
    | VendorGroup   Extension
    | Internal
    deriving(Eq, Ord, Show)

data FuncI = FuncI GLName HsName FType [FType]
    deriving (Eq, Ord, Show)
data EnumI = EnumI GLName HsName ValueType
    deriving (Eq, Ord, Show)

data Reexport
    = SingleExport ModuleName HsName
    | ModuleExport ModuleName
    deriving (Eq, Ord, Show)
