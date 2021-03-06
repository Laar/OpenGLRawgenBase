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

data OpenGLRawI
    = OpenGLRawI
    { rawMods :: Map ModuleName ModuleType
    } deriving ()

data ModuleI
    = ModuleI
    { modName  :: ModuleName
    , modType  :: ModuleType
    , modEnums :: S.Set EnumI
    , modFuncs :: S.Set FuncI
    , modReExports :: S.Set Reexport
    } deriving ()

data FuncI = FuncI GLName HSName FType [FType]
    deriving (Eq, Ord, Show)
data EnumI = EnumI GLName HSName ValueType
    deriving (Eq, Ord, Show)

data Reexport
    = SingleExport ModuleName GLName
    | ModuleExport ModuleName
    deriving (Eq, Ord, Show)
