-- | I'm not sure whether this belongs in th-path.  The HasStack monad
-- used in MIMO to construct lenses that look deep into a record type.
-- However, it does not involve the Path type mechanism, and is
-- unaware of View instances and other things that modify the type
-- graph.  Lets see how it adapts.
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.TH.Path.Stack
    ( Hint(..)
    , HasStack(push, withStack)
    , StackElement(..)
    , prettyStack
    , foldField
      -- * Stack+instance map monad
    , StackT
    , execStackT
      -- * Subtype traversal
    -- , visitSubtypes
    -- , findSubtypes
    -- , evalSubtypes
    -- , enumerateSubtypes
      -- * Stack operations
    , fieldNameString
    , stackAccessor
    , makeLenses
    ) where

import Control.Applicative ((<$>), pure)
import Control.Category ((.))
import Control.Lens (iso, Lens', lens, set, view)
import Control.Monad.Reader (ReaderT, runReaderT, ask, local)
import Control.Monad.RWS (RWST)
import Control.Monad.State (StateT, evalStateT, get)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT, runWriterT, execWriterT, tell)
import Data.Char (isUpper, toUpper)
import Data.Generics (Data, Typeable)
import Data.List (groupBy)
import Data.Map as Map (keys)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid)
import Language.Haskell.Exts.Syntax ()
import Language.Haskell.TH
import Language.Haskell.TH.TypeGraph.Core
    (FieldType(FieldType, fPos, fNameAndType),
     fName, fType, constructorFields, constructorName)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Syntax hiding (lift)
import Language.Haskell.TH.TypeGraph (E(E), etype, simpleEdges, typeGraphInfo, typeGraphEdges)
import Language.Haskell.TH.TypeGraph.Graph (GraphEdges)
import Language.Haskell.TH.TypeGraph.Vertex (TypeGraphVertex)
import Prelude hiding ((.))


import Control.Applicative ((<*>))
import Prelude hiding ((.))

-- | Hints about alternative ways to handle page rendering, returned
-- by the siteHints function in the Site record.  This should be
-- transitioned to the class/instance mechanism used in the View
-- module.
data Hint
    = HideColumn -- ^ In the 'list' or 'table' view, hide the column that would have contained this value
    | Div -- ^ In the single record view, put this value in a div at the bottom with no label
    | HideField  -- ^ In the record view hide this value
    | Link -- ^ Hyperlink this value to the parent record
    | Area -- ^ Make a field a text area
    | TimeStamp -- ^ This UTCTime field will receive the current time when the value is created
    | Flatten -- ^ Don't generate a label for this record, or indent its fields
    | Title String -- ^ Use the given string for the field title
    deriving (Eq, Ord, Show)

-- | The information required to extact a field value from a value.
-- We keep a stack of these as we traverse a declaration.  Generally,
-- we only need the field names.
data StackElement = StackElement FieldType Con Dec deriving (Eq, Show, Data, Typeable)

class Monad m => HasStack m where
    withStack :: ([StackElement] -> m a) -> m a -- Better name: askStack
    push :: FieldType -> Con -> Dec -> m a -> m a -- Better name: localStack

instance (Quasi m, Monoid w) => HasStack (RWST [StackElement] w s m) where
    withStack f = ask >>= f
    push fld con dec action = local (\ stk -> StackElement fld con dec : stk) action

instance HasStack m => HasStack (StateT s m) where
    withStack f = lift (withStack return) >>= f
    push fld con dec action = get >>= \ st -> lift $ push fld con dec (evalStateT action st)

instance Quasi m => HasStack (ReaderT [StackElement] m) where
    withStack f = ask >>= f
    push fld con dec action = local (\ stk -> StackElement fld con dec : stk) action

instance (HasStack m, Monoid w) => HasStack (WriterT w m) where
    withStack f = lift (withStack return) >>= f
    push fld con dec action =
        do (r, w') <- lift $ push fld con dec (runWriterT action)
           tell w'
           return r

prettyStack :: [StackElement] -> String
prettyStack = prettyStack' . reverse
    where
      prettyStack' :: [StackElement] -> String
      prettyStack' [] = "(empty)"
      prettyStack' (x : xs) = "[" ++ prettyElt x ++ prettyTail xs ++ "]"
      prettyTail [] = ""
      prettyTail (x : xs) = " → " ++ prettyElt x ++ prettyTail xs
      prettyElt (StackElement fld con dec) = prettyDec dec ++ ":" ++ prettyCon con ++ "." ++ pprint fld
      prettyDec (TySynD _ _ typ) = prettyType typ
      prettyDec (NewtypeD _ name _ _ _) = nameBase name
      prettyDec (DataD _ name _ _ _) = nameBase name
      prettyDec dec = error $ "prettyStack: " ++ show dec
      prettyCon = nameBase . constructorName
      prettyType (AppT t1 t2) = "((" ++ prettyType t1 ++ ") (" ++ prettyType t2 ++ "))"
      prettyType (ConT name) = nameBase name
      prettyType typ = "(" ++ show typ ++ ")"

-- | Push the stack and process the field.
foldField :: HasStack m => (FieldType -> m r) -> Dec -> Con -> FieldType -> m r
foldField doField dec con fld = push fld con dec $ doField fld

type StackT m = ReaderT [StackElement] m

execStackT :: Monad m => StackT m a -> m a
execStackT action = runReaderT action []

-- | Return a string which describes a record field:
--  1. If there is a field name, use that
--  2. If the constructor has one field, use the constructor name
--  3. If the field is a named type, use it
--  4. Otherwise, use the constructor and the field position
fieldNameString :: (Quasi m, HasStack m) => m [Hint] -> m String
fieldNameString lookHints =
    do hs <- lookHints
       withStack (return . title hs)
    where
      title :: [Hint] -> [StackElement] -> String
      title hs (StackElement fld con dec : _) =
          useFieldPos `fromMaybe` useFieldType `fromMaybe` useConName `fromMaybe` useFieldName `fromMaybe` useHint hs
          where
            useFieldName :: Maybe String
            useFieldName = fmap (camelWords . nameBase) (fName fld)
            useConName :: Maybe String
            useConName = fmap (camelWords . nameBase) $ if constructorCount dec == 1 then Just (constructorName con) else Nothing
            useFieldType :: Maybe String
            useFieldType = fmap (camelWords . nameBase) $ doType (fType fld)
            useFieldPos :: String
            useFieldPos = camelWords (nameBase (constructorName con)) ++ "." ++ maybe (error "elemIndex") show (elemIndex fld (constructorFields con))
            useHint [] = Nothing
            useHint (Title s : _) = Just s
            useHint (_ : more) = useHint more
      title _hs stk = "fieldNameString - bad stack: " ++ prettyStack stk
      doType (ForallT _ _ typ) = doType typ
      doType (ConT name) = Just name
      doType _ = Nothing
      constructorCount :: Dec -> Int
      constructorCount (NewtypeD _ _ _ _ _) = 1
      constructorCount (DataD _ _ _ cons _) = length cons
      constructorCount dec = error $ "constructorCount: " ++ show dec

-- | Convert a camel case string (no whitespace) into a natural
-- language looking phrase:
--   camelWords3 "aCamelCaseFOObar123" -> "A Camel Case FOObar123"
camelWords :: String -> String
camelWords s =
    case groupBy (\ a b -> isUpper a == isUpper b) s of -- "aCamelCaseFOObar123"
      (x : xs) -> concat $ capitalize x : map (\ (c : cs) -> if isUpper c then ' ' : c : cs else c : cs) xs
      [] -> ""

capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = (toUpper c) : cs

-- | Re-implementation of stack accessor in terms of stackLens
stackAccessor :: (Quasi m, HasStack m) => ExpQ -> Type -> m Exp
stackAccessor value typ0 =
    do lns <- withStack (runQ . stackLens)
       typ <- stackType
       runQ [| view $(pure lns) $value :: $(pure (fromMaybe typ0 typ)) |]

stackType :: HasStack m => m (Maybe Type)
stackType =
    withStack (return . f)
    where
      f [] = Nothing
      f (StackElement fld _ _ : _) = Just (fType fld)

-- | Return an expression of a lens for the value described by the
-- stack.
stackLens :: [StackElement] -> Q Exp
stackLens [] = [| iso id id |]
stackLens xs = mapM fieldLens xs >>= foldl1 (\ a b -> [|$b . $a|]) . map return

nthLens :: Int -> Lens' [a] a
nthLens n = lens (\ xs -> xs !! n) (\ xs x -> take (n - 1) xs ++ [x] ++ drop n xs)

-- | Generate a lens to access a field, as represented by the
-- StackElement type.
fieldLens :: StackElement -> Q Exp
fieldLens e@(StackElement fld con _) =
    do lns <-
           case fName fld of
              Just fieldName ->
                  -- Use the field name to build an accessor
                  let lensName = lensNamer (nameBase fieldName) in
                  lookupValueName lensName >>= maybe (error ("fieldLensName - missing lens: " ++ lensName)) varE
              Nothing ->
                  -- Build a pattern expression to extract the field
                  do cname <- lookupValueName (nameBase $ constructorName con) >>= return . fromMaybe (error $ "fieldLens: " ++ show e)
                     f <- newName "f"
                     let n = length $ constructorFields con
                     as <- mapM newName (map (\ p -> "_a" ++ show p) [1..n])
                     [| lens -- \ (Con _ _ _ x _ _) -> x
                             $(lamE [conP cname (set (nthLens (fPos fld)) (varP f) (repeat wildP))] [| $(varE f) :: $(pure (fType fld)) |])
                             -- \ x (Con a b c _ d e) -> Con a b c x d e
                             $(lamE [conP cname (map varP as), varP f] (foldl appE (conE cname) (set (nthLens (fPos fld)) (varE f) (map varE as)))) |]
       [| $(pure lns) {- :: Lens $(pure top) $(pure (fType fld)) -} |]

-- Generate lenses to access the fields of the row types.
makeLenses :: [Dec] -> Q [Dec]
makeLenses decs =
    execWriterT $ execStackT $ typeGraphInfo (concatMap decType decs) >>= runReaderT typeGraphEdges >>= \ (g :: GraphEdges () TypeGraphVertex) -> (mapM doType . map (view etype) . Map.keys . simpleEdges $ g)
    where
      decType (NewtypeD _ name _ _ _) = [ConT name]
      decType (DataD _ name _ _ _) = [ConT name]
      decType (TySynD name _ _) = [ConT name]
      decType _ = []
      doType (E (ConT name)) = qReify name >>= doInfo
      doType _ = return ()
      doInfo (TyConI dec@(NewtypeD _ typeName _ con _)) = doCons dec typeName [con]
      doInfo (TyConI dec@(DataD _ typeName _ cons _)) = doCons dec typeName cons
      doInfo _ = return ()
      doCons dec typeName cons = mapM_ (\ con -> mapM_ (foldField (doField typeName) dec con) (constructorFields con)) cons

      -- (mkName $ nameBase $ tName dec) dec lensNamer) >>= tell
      doField :: Name -> FieldType -> StackT (WriterT [Dec] Q) ()
      doField typeName (FieldType {fNameAndType = Right (fieldName, _, fieldType)}) =
          doFieldType typeName fieldName fieldType
      doField _ _ = return ()
      doFieldType typeName fieldName (ForallT _ _ typ) = doFieldType typeName fieldName typ
      doFieldType typeName fieldName fieldType@(ConT fieldTypeName) = qReify fieldTypeName >>= doFieldInfo typeName fieldName fieldType
      doFieldType typeName fieldName fieldType = makeLens typeName fieldName fieldType
      doFieldInfo typeName fieldName fieldType (TyConI _fieldTypeDec) = makeLens typeName fieldName fieldType
      doFieldInfo _ _ _ (PrimTyConI _ _ _) = return ()
      doFieldInfo _ _ _ info = error $ "makeLenses - doFieldType: " ++ show info

      makeLens typeName fieldName fieldType =
          do let lensName = mkName (lensNamer (nameBase fieldName))
             sig <- runQ $ sigD lensName (runQ [t|Lens' $(conT typeName) $(pure fieldType)|])
             val <- runQ $ valD (varP lensName) (normalB (runQ [|lens $(varE fieldName) (\ s x -> $(recUpdE [|s|] [ (,) <$> pure fieldName <*> [|x|] ]))|])) []
             return [sig, val] >>= tell

-- | Given a field name, return the name to use for the corresponding lens.
lensNamer :: String -> String
lensNamer (n : ame) = "lens" ++ [toUpper n] ++ ame
lensNamer "" = error "Saw the empty string as a field name"
