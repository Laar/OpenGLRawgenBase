module Language.OpenGLRaw.Interface.Query (
    gllookupValues, gllookupBy, GLIndex, mkGLIndex,
) where

import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S

import Language.OpenGLRaw.Base
import Language.OpenGLRaw.Interface.Types

type GLIndex = (OpenGLRawI, M.Map ModuleName ModuleI)

mkGLIndex :: OpenGLRawI -> [ModuleI] -> GLIndex
mkGLIndex rawI mods = (rawI, M.fromList $ map (\m -> (modName m, m)) mods)

openGLRawI :: GLIndex -> OpenGLRawI
openGLRawI = fst

modules :: GLIndex -> M.Map ModuleName ModuleI
modules = snd

class EnumOrFunc ef where
    getValues :: ModuleI -> S.Set ef
    getName   :: ef -> GLName

instance EnumOrFunc EnumI where
    getValues = modEnums
    getName (EnumI n _ _) = n
instance EnumOrFunc FuncI where
    getValues = modFuncs
    getName (FuncI n _ _ _) = n

gllookupValues :: EnumOrFunc ef => GLName -> GLIndex -> Map ModuleName ef
gllookupValues name index
    = M.fromList [e | m <- M.keys $ modules index
                    , Just e <- [recLookup name m index]]

gllookupBy :: EnumOrFunc ef
    => (ModuleType -> Ordering) -> GLName -> GLIndex
    -> Maybe (ModuleName, ef)
gllookupBy order name index
    = getFirst . mconcat . map First
        $ [ recLookup name m index
            | m <- reorder . M.toList . rawMods $ openGLRawI index]
 where reorder = map fst . sortBy (compare `on` (order . snd))

recLookup :: EnumOrFunc ef
    => GLName -> ModuleName -> GLIndex -> Maybe (ModuleName, ef)
recLookup n startMod index = (,) startMod `fmap` go [startMod]
  where
    go [] = Nothing
    go (m:ms)  = case M.lookup m . modules $ index of
        Nothing   -> go ms
        Just modu -> case find ((n ==) . getName) . S.toList $ getValues modu of
            Just val -> Just val
            Nothing  ->
                let impMods = mapMaybe reexport . S.toList $ modReExports modu
                    reexport :: Reexport -> Maybe ModuleName
                    reexport (ModuleExport mn   ) = Just mn
                    reexport (SingleExport mn n') | n' == n   = Just mn
                                                  | otherwise = Nothing
                in go (impMods ++ ms)
