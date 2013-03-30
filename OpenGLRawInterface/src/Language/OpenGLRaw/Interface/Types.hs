module Language.OpenGLRaw.Interface.Types (
    OpenGLRawI(..),
    ModuleI(..), ModuleType(..),
    Major, Minor, Deprecated,
    FuncI(..), EnumI(..), Reexport(..),

    -- * Convenience reexport
    ModuleName(..),
) where

import Data.Map (Map)
import qualified Data.Set as S

import Language.Haskell.Exts.Syntax(ModuleName(..))
import Language.OpenGLRaw.Base

type HsName = String

data OpenGLRawI
    = OpenGLRawI
    { rawMods :: Map ModuleName ModuleType
    } deriving (Show)

data ModuleI
    = ModuleI
    { modName  :: ModuleName
    , modType  :: ModuleType
    , modEnums :: S.Set EnumI
    , modFuncs :: S.Set FuncI
    , modReExports :: S.Set Reexport
    } deriving (Show)

data FuncI = FuncI GLName HsName FType [FType]
    deriving (Eq, Ord, Show)
data EnumI = EnumI GLName HsName ValueType
    deriving (Eq, Ord, Show)

data Reexport
    = SingleExport ModuleName GLName
    | ModuleExport ModuleName
    deriving (Eq, Ord, Show)
