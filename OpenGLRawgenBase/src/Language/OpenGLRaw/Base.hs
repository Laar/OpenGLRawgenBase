module Language.OpenGLRaw.Base (
    GLName, FType(..), ValueType(..),

    Category(..), Extension(..),
    showCategory,
) where

import Text.OpenGL.Spec(Category(..), Extension(..), showCategory)

-- | The original name of something from OpenGL (thus the name as used in the
-- specification).
type GLName = String

data ValueType
    = EnumValue
    | BitfieldValue
    deriving (Eq, Ord, Show)


-- | Simple typing, sufficient for OpenGL functions.
data FType
    = TCon String
    | TVar
    | TPtr FType
    | UnitTCon
    deriving (Eq, Ord, Show)
