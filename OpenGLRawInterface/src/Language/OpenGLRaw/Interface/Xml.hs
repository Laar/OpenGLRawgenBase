{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.OpenGLRaw.Interface.Xml (
    GLXml(..),
) where

import Control.Applicative
import Control.Monad
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
        pure . OpenGLRawI $ listed moduleElement e
        where
            moduleElement :: Element -> Either String (M.Map ModuleName ModuleType)
            moduleElement = guardName "module" $ \e' ->
                M.singleton
                <$> fmap ModuleName (findAttr' "name" e')
                <*> singleGLChild e'

instance GLXml ModuleI where
    toGLXml (ModuleI (ModuleName mname) mType enums funcs reexports) =
        node "module"
            ([ Attr "name" mname
             ]
            ,[ node "moduletype" $ toGLXml mType
             , foldElements "enums" enums
             , foldElements "functions" funcs
             , foldElements "reexports" reexports
             ])
    fromGLXml = guardName "module" $ \e ->
        ModuleI . ModuleName
        <$> findAttr' "name" e
        <*> (singleNamedChild "moduletype" e >>= singleGLChild)
        <*> listedGLUnder "enums"       S.singleton e
        <*> listedGLUnder "functions"   S.singleton e
        <*> listedGLUnder "reexports"   S.singleton e

listed :: Monoid m => (Element -> Either String m) -> Element -> m
listed f = F.fold . rights . map f . elChildren

listedUnder :: Monoid m => QName -> (Element -> Either String m) -> Element -> Either String m
listedUnder n f e = maybeNamedChild n (pure . listed f) e

listedGLUnder :: (Monoid m, GLXml t) => QName -> (t -> m) -> Element -> Either String m
listedGLUnder n f e = listedUnder n (fmap f . fromGLXml) e

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
                [ Attr "vendor"     $ showCompExtension vendor
                , Attr "name"       $ name
                , Attr "deprecated" $ show d
                ]
        TopLevelGroup       -> node "toplevelgroup" ()
        VendorGroup vendor  -> node "vendorgroup" [Attr "vendor" $ showCompExtension vendor]
        Compatibility       -> node "compatibility" ()
        Internal            -> node "internal" ()
      where
    fromGLXml e = case elName e of
        "core"          ->
            CoreInterface
            <$> findReadAttr "major" e
            <*> findReadAttr "minor" e
            <*> findReadAttr "deprecated" e
        "extension"     ->
            ExtensionMod
            <$> (readCompExtension <$> findAttr' "vendor" e)
            <*> findAttr' "name" e
            <*> findReadAttr "deprecated" e
        "toplevelgroup" -> pure TopLevelGroup
        "vendorgroup"   -> VendorGroup . readCompExtension <$> findAttr' "vendor" e
        "compatibility" -> pure Compatibility
        "internal"      -> pure Internal
        n               -> Left $ "Not an ModuleType: " ++ showQName n

instance GLXml FuncI where
    toGLXml (FuncI gln hsn rt ats) =
        node "function"
            ([ glNameAttr gln
             , hsNameAttr hsn
             ]
            ,[ node "return" $ toGLXml rt
             , node "arguments" $ map toGLXml ats
             ])
    fromGLXml = guardName "function" $ \e ->
        FuncI
        <$> findGLName e
        <*> findHSName e
        <*> (singleNamedChild "return" e >>= oneGLChild)
        <*> listedGLUnder "arguments" (:[]) e

instance GLXml EnumI where
    toGLXml (EnumI gln hsn vt) =
        node "enum"
            [ glNameAttr gln
            , hsNameAttr hsn
            , Attr "valuetype" $ valueTypeToString vt
            ]
    fromGLXml = guardName "enum" $ \e ->
        EnumI
        <$> findGLName e
        <*> findHSName e
        <*> (findAttr' "valuetype" e >>= stringToValueType)

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
                , glNameAttr gln
                ]
        ModuleExport (ModuleName m) ->
            node "mreexport"
                [ Attr "module" m
                ]
    fromGLXml e = case elName e of
        "sreexport" -> SingleExport . ModuleName
                <$> findAttr' "module" e
                <*> findGLName e
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
        "ptr"   -> TPtr <$> oneGLChild e
        "unit"  -> pure UnitTCon
        _       -> Left "No FType element"

findGLName :: Element -> Either String GLName
findGLName e = GLName <$> findAttr' "glname" e
findHSName :: Element -> Either String HSName
findHSName e = Ident <$> findAttr' "hsname" e
glNameAttr :: GLName -> Attr
glNameAttr gln = Attr "glname" $ unGLName gln
hsNameAttr :: HSName -> Attr
hsNameAttr = Attr "hsname" . unHSName

singleChild :: Element -> Either String Element
singleChild e = case elChildren e of
    [c] -> pure c
    []  -> Left "No children"
    _   -> Left "More than one child"

maybeNamedChild :: Monoid m => QName -> (Element -> Either String m)
    -> Element -> Either String m
maybeNamedChild n f e = case findChildren n e of
    []  -> pure mempty
    [c] -> f c
    _   -> Left $ "More than one child with name " ++ showQName n

singleGLChild :: GLXml t => Element -> Either String t
singleGLChild = singleChild >=> fromGLXml

singleNamedChild :: QName -> Element -> Either String Element
singleNamedChild q e = case findChildren q e of
    [c] -> pure c
    []  -> Left $ "No child with name " ++ showQName q
    _   -> Left $ "More than one child with name " ++ showQName q

oneGLChild :: GLXml t => Element -> Either String t
oneGLChild e = case rights . map fromGLXml $ elChildren e of
    [c] -> pure c
    []  -> Left "No correct GLXml child"
    _   -> Left "More than one correct GLXml child"

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
