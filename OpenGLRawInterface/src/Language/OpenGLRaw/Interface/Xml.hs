{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.OpenGLRaw.Interface.Xml (
) where

import Control.Applicative
import qualified Data.Map as M
import qualified Data.Set as S
import Text.XML.HXT.Core

import Language.OpenGLRaw.Base
import Language.OpenGLRaw.Interface.Types

instance XmlPickler OpenGLRawI where
    xpickle = xpElem "rawPackage"
        $ xpWrap (OpenGLRawI, \(OpenGLRawI mods) -> mods)
        $ xpMapPair xpModule
      where
        xpModule = xpElem "module"
            $ xpPair
                (xpModuleName "name")
                xpickle

instance XmlPickler ModuleI where
    xpickle = xpElem "module"
        $ xpWrap (\(mn, mty, enums, funcs, reexports) -> ModuleI mn mty enums funcs reexports,
                \(ModuleI mn mty enums funcs reexports) -> (mn, mty, enums, funcs, reexports))
        $ xp5Tuple
            (xpModuleName "name")
            (xpElem "moduletype" xpickle)
            (xpElem "enums"     $ xpSet xpickle)
            (xpElem "functions" $ xpSet xpickle)
            (xpElem "reexports" $ xpSet xpickle)

xpSet :: Ord a => PU a -> PU (S.Set a)
xpSet = xpWrap (S.fromList, S.toList) . xpList
xpMapPair :: Ord k => PU (k,v) -> PU (M.Map k v)
xpMapPair = xpWrap (M.fromList, M.toList) . xpList

instance XmlPickler ModuleType where
    xpickle = xpAlt tag ps
      where
        tag mt = case mt of
            CoreInterface _ _ _ -> 0
            ExtensionMod  _ _ _ -> 1
            TopLevelGroup       -> 2
            VendorGroup   _     -> 3
            Compatibility       -> 4
            Internal            -> 5
        ps =
            [ xpCore
            , xpExtension
            , xpElem "toplevelgroup" $ xpLift TopLevelGroup
            , xpElem "vendorgroup" 
                $ xpWrap (VendorGroup, \(VendorGroup v) -> v) 
                $ xpVendor
            , xpElem "compatibility" $ xpLift Compatibility
            , xpElem "internal"      $ xpLift Internal
            ]
        xpCore = xpElem "core"
            $ xpWrap (uncurry3 CoreInterface,
                    \(CoreInterface ma mi p)  -> (ma, mi, p))
            $ xpTriple
                (xpAttr "major" xpickle)
                (xpAttr "minor" xpickle)
                xpProfile
        xpExtension = xpElem "extension"
            $ xpWrap (uncurry3 ExtensionMod, \(ExtensionMod v n p) -> (v, n, p))
            $ xpTriple
                xpVendor
                (xpTextAttr "name")
                xpProfile
        xpProfile :: PU Profile                
        xpProfile = xpWrap (p, up) $ xpTextAttr "profile"
          where
            up prof = case prof of
                DefaultProfile  -> "default"
                ProfileName n   -> n
            p s = case s of
                "default"   -> DefaultProfile
                n           -> ProfileName n
        xpVendor :: PU Vendor
        xpVendor = xpWrap (Vendor, vendorName) $ xpTextAttr "vendor"

instance XmlPickler FuncI where
    xpickle = xpElem "function"
        $ xpWrap (uncurry4 FuncI, \(FuncI gln hsn rt ats) -> (gln, hsn, rt, ats))
        $ xp4Tuple
            xpGLName
            xpHSName
            (xpElem "return" $ xpickle)
            (xpElem "arguments" $ xpList xpickle)

instance XmlPickler EnumI where
    xpickle = xpElem "enum"
        $ xpWrap (uncurry3 EnumI, \(EnumI gln hsn vt) -> (gln, hsn, vt))
        $ xpTriple
            xpGLName
            xpHSName
            xpValueType
      where
        xpValueType = xpWrapEither (stringToValueType, valueTypeToString)
                        $ xpTextAttr "valuetype"
        valueTypeToString :: ValueType -> String
        valueTypeToString vt = case vt of
            EnumValue       -> "enum"
            BitfieldValue   -> "bitfield"
        stringToValueType :: String -> Either String ValueType
        stringToValueType s = case s of
            "enum"      -> pure EnumValue
            "bitfield"  -> pure BitfieldValue
            _           -> Left $ "Invalid valuetype " ++ s
instance XmlPickler Reexport where
    xpickle = xpAlt tag ps
      where
        tag r = case r of
            SingleExport _ _    -> 0
            ModuleExport _      -> 1
        ps =
            [ xpWrap (uncurry SingleExport, \(SingleExport m gln) -> (m, gln))
                $ xpElem "sreexport"
                $ xpPair
                    (xpModuleName "module")
                    xpGLName
            , xpWrap (ModuleExport, \(ModuleExport m) -> m)
                $ xpElem "mreexport"
                $ xpModuleName "module"
            ]


instance XmlPickler FType where
    xpickle = xpAlt tag ps
      where
        tag (TCon _) = 0
        tag TVar     = 1
        tag (TPtr _) = 2
        tag UnitTCon = 3
        ps =
            [ xpElem "con"  $ xpWrap (TCon, \(TCon t) -> t) $ xpTextAttr "name"
            , xpElem "var"  $ xpLift TVar
            , xpElem "ptr"  $ xpWrap (TPtr, \(TPtr p) -> p) xpickle
            , xpElem "unit" $ xpLift UnitTCon
            ]
xpModuleName :: String -> PU ModuleName
xpModuleName = xpWrap (ModuleName, \(ModuleName m) -> m) . xpTextAttr
xpGLName :: PU GLName
xpGLName = xpWrap (GLName, unGLName) $ xpTextAttr "glname"
xpHSName :: PU HSName
xpHSName = xpWrap (Ident, unHSName) $ xpTextAttr "hsname"
