{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.OpenGLRaw.Interface.Xml (
    GLXml(..),
) where

import Control.Applicative
import Data.Either
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import Data.String
import Text.XML.Light

import Language.OpenGLRaw.Base
import Language.OpenGLRaw.Interface.Types

class GLXml t where
    toGLXml   :: t -> Element
    fromGLXml :: Element -> Either String t

-- conveniece instance
instance IsString QName where
    fromString = unqual

foldElements :: (F.Foldable f, GLXml l) => QName -> f l -> Element
foldElements qn = node qn . F.foldMap ((:[]) . toGLXml)

instance GLXml OpenGLRawI where
    toGLXml (OpenGLRawI mods) =
        node "rawPackage" $ map moduleElement $ M.toList mods
        where moduleElement (ModuleName mname, ty)
                = node "module" ([Attr "name" mname], toGLXml ty)
    fromGLXml = guardName "rawPackage" $ \e ->
        OpenGLRawI
        <$> (pure . unfoldElements "module" e $ \e' ->
            M.singleton 
            <$> fmap ModuleName (findAttr' "name" e')
            <*> singleChildGL e')

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
    fromGLXml = guardName "module" $ \e ->
        ModuleI . ModuleName
        <$> findAttr' "name" e
        <*> (singleChild "moduletype" e >>= single . elChildren)
        <*> pure (unfoldElements' "enums"      e S.singleton)
        <*> pure (unfoldElements' "functions"  e S.singleton)
        <*> pure (unfoldElements' "reexports"  e S.singleton)

unfoldElements :: Monoid m => QName -> Element -> (Element -> Either String m) -> m
unfoldElements n e f = F.fold . rights . map f $ findChildren n e

unfoldElements' :: (Monoid m, GLXml t) => QName -> Element -> (t -> m) -> m
unfoldElements' n e f = unfoldElements n e $ fmap f . fromGLXml

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
        Compatibility       -> node "compatibility" ()
        Internal            -> node "internal" ()
    fromGLXml e = case elName e of
        "core"          ->
            CoreInterface
            <$> findReadAttr "major" e
            <*> findReadAttr "minor" e
            <*> findReadAttr "deprecated" e
        "extension"     ->
            ExtensionMod
            <$> findReadAttr "vendor" e
            <*> findAttr' "name" e
            <*> findReadAttr "deprecated" e
        "toplevelgroup" -> pure TopLevelGroup
        "vendorgroup"   -> VendorGroup <$> findReadAttr "vendor" e
        "compatibility" -> pure Compatibility
        "internal"      -> pure Internal
        n               -> Left $ "Not an ModuleType: " ++ showQName n

instance GLXml FuncI where
    toGLXml (FuncI gln hsn rt ats) =
        node "function"
            ([ Attr "glname" gln
             , Attr "hsname" hsn
             ]
            ,[ node "return" $ toGLXml rt
             , node "arguments" $ map toGLXml ats
             ])
    fromGLXml = guardName "function" $ \e ->
        FuncI
        <$> findAttr' "glname" e
        <*> findAttr' "hsname" e
        <*> singleChild' "return" e
        <*> (pure . rights . map fromGLXml $ findChildren "arguments" e)

instance GLXml EnumI where
    toGLXml (EnumI gln hsn vt) =
        node "enum"
            [ Attr "glname" gln
            , Attr "hsname" hsn
            , Attr "valuetype" $ valueTypeToString vt
            ]
    fromGLXml = guardName "enum" $ \e ->
        EnumI
        <$> findAttr' "glName" e
        <*> findAttr' "hsname" e
        <*> (findAttr' "valueType" e >>= stringToValueType)

valueTypeToString :: ValueType -> String
valueTypeToString vt = case vt of
    EnumValue       -> "enum"
    BitfieldValue   -> "bitfield"
stringToValueType :: String -> Either String ValueType
stringToValueType s = case s of
    "enum"      -> pure EnumValue
    "bitfield"  -> pure BitfieldValue
    _           -> Left $ "Invalid valuetype " ++ s

instance GLXml Reexport where
    toGLXml r = case r of
        SingleExport (ModuleName m) gln ->
            node "sreexport"
                [ Attr "module" m
                , Attr "glname" gln
                ]
        ModuleExport (ModuleName m) ->
            node "mreexport"
                [ Attr "module" m
                ]
    fromGLXml e = case elName e of
        "sreexport" -> SingleExport . ModuleName
                <$> findAttr' "module" e
                <*> findAttr' "glname" e
        "mreexport" -> ModuleExport . ModuleName <$> findAttr' "module" e
        _           -> Left "No Reexport element"

instance GLXml FType where
    toGLXml ft = case ft of
        TCon s      -> node "con" [Attr "name" s]
        TVar        -> node "var" ()
        TPtr p      -> node "ptr" $ toGLXml p
        UnitTCon    -> node "unit" ()
    fromGLXml e = case elName e of
        "con"   -> TCon <$> findAttr' "name" e
        "var"   -> pure TVar
        "ptr"   -> TPtr <$> single (elChildren e)
        "unit"  -> pure UnitTCon
        _       -> Left "No FType element"

single :: GLXml t => [Element] -> Either String t
single els = case rights $ map fromGLXml els of
    [x] -> pure x
    []  -> Left "No such element"
    _   -> Left "More then on element"

singleChild :: QName -> Element -> Either String Element
singleChild q e = case findChildren q e of
    [x] -> pure x
    []  -> Left $ "No child with name: " ++ showQName q
    _   -> Left $ "More then on child with name " ++ showQName q

singleChild' :: GLXml t => QName -> Element -> Either String t
singleChild' n e = singleChild n e >>= fromGLXml

singleChildGL :: GLXml t => Element -> Either String t
singleChildGL e = case rights . map fromGLXml $ elChildren e of
    [x] -> pure x
    []  -> Left "No correct children"
    _   -> Left "More than one correct child"
    

guardName :: QName -> (Element -> Either String t) -> Element -> Either String t
guardName n f e = 
    if (elName e == n) 
        then f e
        else Left $ "Not an " ++ showQName n

findAttr' :: QName -> Element -> Either String String
findAttr' q = 
    liftMaybe ("Attribute " ++ showQName q ++ " not found") . findAttr q

findReadAttr :: Read r => QName -> Element -> Either String r
findReadAttr n e = findAttr' n e >>= tryRead

tryRead :: Read a => String -> Either String a
tryRead = liftMaybe "Reader failed". fmap fst . listToMaybe . reads

liftMaybe :: String -> Maybe t -> Either String t
liftMaybe msg = maybe (Left msg) pure
