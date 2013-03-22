{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.OpenGLRaw.Interface.Xml (
    GLXml(..),
) where

import qualified Data.Foldable as F
import Data.String
import Text.XML.Light

import Language.OpenGLRaw.Base
import Language.OpenGLRaw.Interface.Types

class GLXml t where
    toGLXml :: t -> Element

-- conveniece instance
instance IsString QName where
    fromString = unqual

foldElements :: (F.Foldable f, GLXml l) => QName -> f l -> Element
foldElements qn = node qn . F.foldMap ((:[]) . toGLXml)

instance GLXml OpenGLRawI where
    toGLXml (OpenGLRawI mods) =
        node "rawPackage" $ F.foldMap moduleElement mods
        where moduleElement (ModuleName mname)
                = [node "module" [Attr "name" mname]]

instance GLXml ModuleI where
    toGLXml (ModuleI (ModuleName mname) mType enums funcs reexports) =
        node "module"
            ([ Attr "name" mname
             ]
            ,[ node "moduletype" $ toGLXml mType
             , foldElements "enums" enums
             , foldElements "funtions" funcs
             , foldElements "reexports" reexports
             ])

instance GLXml ModuleType where
    toGLXml mt = case mt of
        CoreInterface ma mi d ->
            node "core"
                [ Attr "major"      $ show ma
                , Attr "minor"      $ show mi
                , Attr "deprecated" $ show d
                ]
        ExtensionMod vendor name d ->
            node "extension"
                [ Attr "vendor"     $ show vendor
                , Attr "name"       $ name
                , Attr "deprecated" $ show d
                ]
        TopLevelGroup       -> node "toplevelgroup" ()
        VendorGroup vendor  -> node "vendorgroup" [Attr "vendor" $ show vendor]
        Internal            -> node "internal" ()

instance GLXml FuncI where
    toGLXml (FuncI gln hsn rt ats) =
        node "function"
            ([ Attr "glname" gln
             , Attr "hsname" hsn
             ]
            ,[ node "return" $ toGLXml rt
             , node "arguments" $ map toGLXml ats
             ])

instance GLXml EnumI where
    toGLXml (EnumI gln hsn vt) =
        node "enum"
            [ Attr "glname" gln
            , Attr "hsname" hsn
            , Attr "valuetype" $ valueTypeToString vt
            ]

valueTypeToString :: ValueType -> String
valueTypeToString vt = case vt of
    EnumValue       -> "enum"
    BitfieldValue   -> "bitfield"

instance GLXml Reexport where
    toGLXml r = case r of
        SingleExport (ModuleName m) hsn ->
            node "sreexport"
                [ Attr "module" m
                , Attr "value" hsn
                ]
        ModuleExport (ModuleName m) ->
            node "mreexport"
                [ Attr "module" m
                ]

instance GLXml FType where
    toGLXml ft = case ft of
        TCon s      -> node "con" [Attr "name" s]
        TVar        -> node "var" ()
        TPtr p      -> node "ptr" $ toGLXml p
        UnitTCon    -> node "unit" ()
