module Language.OpenGLRaw.Base (
    GLName(..), FType(..), ValueType(..), ModuleType(..),
    HSName, unHSName, Name(..),

    Major, Minor, Deprecated,

    Vendor(..), Profile(..),
    Category(..),

    showCategory,
) where

import Data.Char(toUpper)

import Language.Haskell.Exts.Syntax(Name(..))

-- | The original name of something from OpenGL (thus the name as used in the
-- specification).
newtype GLName = GLName { unGLName :: String }
    deriving (Eq, Ord, Show)

-- | Simple type alias to make it clear that the name is an haskell identifier.
type HSName = Name

-- | Unwrapping the name into a string for final use.
unHSName :: HSName -> String
unHSName (Ident i) = i
unHSName (Symbol s) = s

data ValueType
    = EnumValue
    | BitfieldValue
    deriving (Eq, Ord, Show)

newtype Vendor = Vendor { vendorName :: String }
    deriving (Eq, Ord)
-- | A profile of an extension or a specific OpenGL version.
data Profile
    -- | The default profile. Which is used to represent either the core
    -- profile or the complete lack of any profile.
    = DefaultProfile
    -- | A specific profile identified by its name.
    | ProfileName String
    deriving (Eq, Ord)

profileTokenSuffix :: Profile -> String
profileTokenSuffix p = case p of
    DefaultProfile -> ""
    ProfileName n  -> '_': map toUpper n

data Category
    = Version Int Int Profile
    | Extension Vendor String Profile
    deriving (Eq, Ord)

showCategory :: Category -> String
showCategory c = case c of
    Version ma mi prof
        -> "VERSION_" ++ show ma ++ "_" ++ show mi ++ profileTokenSuffix prof
    Extension vendor name prof
        -> vendorName vendor ++ "_" ++ name ++ profileTokenSuffix prof

-- | Major version number of OpenGL
type Major = Int
-- | Minor version number of OpenGL
type Minor = Int
-- | Deprecation flag
type Deprecated = Bool

-- | The type of a Module generated by OpenGLRaw
data ModuleType
    -- | A module defining one of the Core profiles
    = CoreInterface Major Minor Profile
    -- | A module defining an extension
    | ExtensionMod  Vendor String Profile
    -- | A grouping module not tied to a specific `Extension` (Vendor)
    | TopLevelGroup
    -- | A grouping module for a specific `Extension` (Vendor)
    | VendorGroup   Vendor
    -- | A module for compatibility between OpenGLRaw versions.
    | Compatibility
    -- | A module for internal use.
    | Internal
    deriving(Eq, Ord)

-- | Simple typing, sufficient for OpenGL functions.
data FType
    = TCon String
    | TVar
    | TPtr FType
    | UnitTCon
    deriving (Eq, Ord, Show)
