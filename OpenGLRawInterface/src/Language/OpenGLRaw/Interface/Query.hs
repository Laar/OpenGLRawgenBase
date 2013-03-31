module Language.OpenGLRaw.Interface.Query (
    glLookup, glLookupFrom,
    ModulePart(),
) where

import Control.Arrow((&&&))
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S


import Language.OpenGLRaw.Base
import Language.OpenGLRaw.Interface.Types

class ModulePart p where
    glName :: p         -> GLName
    index  :: ModuleI   -> S.Set p
instance ModulePart EnumI where
    glName (EnumI gln _ _) = gln
    index   = modEnums
instance ModulePart FuncI where
    glName (FuncI gln _ _ _) = gln
    index   = modFuncs

indexLookup :: ModulePart p => GLName -> ModuleI -> Maybe p
indexLookup gln = F.find ((==) gln . glName) . index

type Result p = (p, ModuleI)
type ResultMap p = M.Map ModuleName (Maybe (Result p))

-- | See `glLookupFrom`
glLookup :: ModulePart p => GLName -> [ModuleI] -> [Result p]
glLookup gln mods = glLookupFrom gln mods mods

-- | Looks up a `ModulePart` from several `ModuleI`s.
glLookupFrom :: (F.Foldable f, ModulePart p)
    => GLName -- ^ What to look for
    -> f ModuleI -- ^ The modules in which it could be defined
    -> [ModuleI] -- ^ The modules from which to report resutls
    -> [Result p]
glLookupFrom gln mods searchFrom =
    flip runReaderT modMap $
    flip evalStateT M.empty $ do
        modu <- listSearcher $ searchFrom
        res <- search gln modu
        maybe mzero return $ res
    where
        modMap = M.fromList . map (modName &&& id) $ F.toList mods

type Searcher p = StateT (ResultMap p) (ReaderT (M.Map ModuleName ModuleI) [])

listSearcher :: [a] -> Searcher p a
listSearcher = lift . lift

-- | The main search function which searches a specific module and where needed
-- its `ReExports`.
search :: ModulePart p => GLName -> ModuleI -> Searcher p (Maybe (Result p))
search gln modu = gets (M.lookup mName) >>= \rv -> case rv of
    Just r -> return r -- already found in a previous pass
    Nothing -> case indexLookup gln modu of
        Just v -> result v -- defined in this module
        Nothing -> do
            -- sift through the ReExports, the code assumes that there is only
            -- one ReExport for the specific glName. Though it does not matter
            -- if there are multiple with the same FuncI/EnumI
            re <- listSearcher . S.toList $ modReExports modu
            mn <- case re of
                ModuleExport mn      -> return mn
                SingleExport mn gln' -> guard (gln' == gln) >> return mn
            reResult =<< searchByModName mn
    where
        mName    = modName modu
        result v = do
            let res = Just (v, modu)
            modify $ M.insert mName res
            return res
        reResult = maybe mzero (result . fst)
        searchByModName mn = asks (M.lookup mn) >>= maybe mzero (search gln)
