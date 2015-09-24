-- | Use template haskell functions to generate the path types for appraisalscribe.
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
-- The generated toLens instances will have incomplete patterns where
-- we tried to generate a clause but we found no path to the goal type.
module Appraisal.ReportPaths where

import Control.Monad.States (evalStateT)
import Control.Monad.Writer (execWriterT)
import Data.Monoid ((<>))
import Debug.Trace
import Language.Haskell.TH
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Path.Graph (runTypeGraphT, S(S))
import Language.Haskell.TH.Path.Instances
import Language.Haskell.TH.Path.Lens
import Language.Haskell.TH.Path.Types
import System.Exit
-- import System.FilePath.Extra (friendlyPrint)
import Test.HUnit
import Common (fixStringLits, stripNames)

decs :: [Dec]
decs = $(do (Just dec) <- lookupTypeName "Dec"
            decs <- evalStateT (execWriterT (runTypeGraphT (pathTypes >> pathLenses >> pathInstances) [ConT dec])) (S mempty mempty)
            lift decs)

decs' = (fixStringLits . stripNames) decs

main :: IO ()
main = do
  mapM_ (putStrLn . show . pprint) decs
  r <- runTestTT (test01 decs')
  case r of
    Counts {errors = 0, failures = 0} -> exitWith ExitSuccess
    _ -> error $ showCounts r

test01 :: [Dec] -> Test
test01 actual =
    let expected = [] in
    TestCase $ assertEqual "foo" expected (map (\d -> trace (show (pprint d)) d) actual)

exepected =
 [d|
    data Path_String a
        = Path_String
        deriving (Eq, Ord, Read, Show, Typeable, Data)
    instance IdPath (Path_String a)
        where idPath = Path_String
    data Path_Rational a
        = Path_Rational
        deriving (Eq, Ord, Read, Show, Typeable, Data)
    instance IdPath (Path_Rational a)
        where idPath = Path_Rational
    type Path_Cxt a = Path_List (Path_Type a)
    type Path_FieldExp a = Path_Pair (Path_Name a) (Path_Exp a)
    type Path_FieldPat a = Path_Pair (Path_Name a) (Path_Pat a)
    type Path_Kind a = Path_Type a
    type Path_Pred a = Path_Type a
    type Path_Type a = Path_Type a
    type Path_Kind a = Path_Type a
    type Path_Pred a = Path_Type a
    type Path_Type a = Path_Type a
    type Path_StrictType a = Path_Pair (Path_Strict a) (Path_Type a)
    data Path_VarStrictType a
        = Path_VarStrictType
        deriving (Eq, Ord, Read, Show, Typeable, Data)
    instance IdPath (Path_VarStrictType a)
        where idPath = Path_VarStrictType
    class HasWord8 c
        where lens_word8 :: Lens' c Word8
    instance HasWord8 Word8
        where lens_word8 = id
    class HasChar c
        where lens_char :: Lens' c Char
    instance HasChar Char
        where lens_char = id
    class HasInt c
        where lens_int :: Lens' c Int
    instance HasInt Int
        where lens_int = id
    class HasBigNat c
        where lens_bigNat :: Lens' c BigNat
    instance HasBigNat BigNat
        where lens_bigNat = id
    class HasInteger c
        where lens_integer :: Lens' c Integer
    instance HasInteger Integer
        where lens_integer = id
    class HasAnnTarget c
        where lens_annTarget :: Lens' c AnnTarget
    instance HasAnnTarget AnnTarget
        where lens_annTarget = id
    class HasBody c
        where lens_body :: Lens' c Body
    instance HasBody Body
        where lens_body = id
    class HasCallconv c
        where lens_callconv :: Lens' c Callconv
    instance HasCallconv Callconv
        where lens_callconv = id
    class HasClause c
        where lens_clause :: Lens' c Clause
    instance HasClause Clause
        where lens_clause = id
    class HasCon c
        where lens_con :: Lens' c Con
    instance HasCon Con
        where lens_con = id
    class HasDec c
        where lens_dec :: Lens' c Dec
    instance HasDec Dec
        where lens_dec = id
    class HasExp c
        where lens_exp :: Lens' c Exp
    instance HasExp Exp
        where lens_exp = id
    class HasFamFlavour c
        where lens_famFlavour :: Lens' c FamFlavour
    instance HasFamFlavour FamFlavour
        where lens_famFlavour = id
    class HasFixity c
        where lens_fixity :: Lens' c Fixity
    instance HasFixity Fixity
        where lens_fixity = id
    class HasFixityDirection c
        where lens_fixityDirection :: Lens' c FixityDirection
    instance HasFixityDirection FixityDirection
        where lens_fixityDirection = id
    class HasForeign c
        where lens_foreign :: Lens' c Foreign
    instance HasForeign Foreign
        where lens_foreign = id
    class HasFunDep c
        where lens_funDep :: Lens' c FunDep
    instance HasFunDep FunDep
        where lens_funDep = id
    class HasGuard c
        where lens_guard :: Lens' c Guard
    instance HasGuard Guard
        where lens_guard = id
    class HasInline c
        where lens_inline :: Lens' c Inline
    instance HasInline Inline
        where lens_inline = id
    class HasLit c
        where lens_lit :: Lens' c Lit
    instance HasLit Lit
        where lens_lit = id
    class HasMatch c
        where lens_match :: Lens' c Match
    instance HasMatch Match
        where lens_match = id
    class HasModName c
        where modName :: Lens' c ModName
    instance HasModName ModName
        where modName = id
    class HasName c
        where lens_name :: Lens' c Name
    instance HasName Name
        where lens_name = id
    class HasNameFlavour c
        where lens_nameFlavour :: Lens' c NameFlavour
    instance HasNameFlavour NameFlavour
        where lens_nameFlavour = id
    class HasNameSpace c
        where lens_nameSpace :: Lens' c NameSpace
    instance HasNameSpace NameSpace
        where lens_nameSpace = id
    class HasOccName c
        where occName :: Lens' c OccName
    instance HasOccName OccName
        where occName = id
    class HasPat c
        where lens_pat :: Lens' c Pat
    instance HasPat Pat
        where lens_pat = id
    class HasPhases c
        where lens_phases :: Lens' c Phases
    instance HasPhases Phases
        where lens_phases = id
    class HasPkgName c
        where pkgName :: Lens' c PkgName
    instance HasPkgName PkgName
        where pkgName = id
    class HasPragma c
        where lens_pragma :: Lens' c Pragma
    instance HasPragma Pragma
        where lens_pragma = id
    class HasRange c
        where lens_range :: Lens' c Range
    instance HasRange Range
        where lens_range = id
    class HasRole c
        where lens_role :: Lens' c Role
    instance HasRole Role
        where lens_role = id
    class HasRuleBndr c
        where lens_ruleBndr :: Lens' c RuleBndr
    instance HasRuleBndr RuleBndr
        where lens_ruleBndr = id
    class HasRuleMatch c
        where lens_ruleMatch :: Lens' c RuleMatch
    instance HasRuleMatch RuleMatch
        where lens_ruleMatch = id
    class HasSafety c
        where lens_safety :: Lens' c Safety
    instance HasSafety Safety
        where lens_safety = id
    class HasStmt c
        where lens_stmt :: Lens' c Stmt
    instance HasStmt Stmt
        where lens_stmt = id
    class HasStrict c
        where lens_strict :: Lens' c Strict
    instance HasStrict Strict
        where lens_strict = id
    class HasTyLit c
        where lens_tyLit :: Lens' c TyLit
    instance HasTyLit TyLit
        where lens_tyLit = id
    class HasTySynEqn c
        where lens_tySynEqn :: Lens' c TySynEqn
    instance HasTySynEqn TySynEqn
        where lens_tySynEqn = id
    class HasTyVarBndr c
        where lens_tyVarBndr :: Lens' c TyVarBndr
    instance HasTyVarBndr TyVarBndr
        where lens_tyVarBndr = id
    class HasType c
        where lens_type :: Lens' c Type
    instance HasType Type
        where lens_type = id
    instance Path ((Guard, Exp)) ((Guard, Exp))
        where type PathType ((Guard, Exp))
                            ((Guard, Exp)) = Path_Pair (Path_Guard ((Guard, Exp)))
                                                       (Path_Exp ((Guard, Exp)))
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal (Guard, Exp) for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) (Maybe Exp)
        where type PathType ((Guard, Exp))
                            (Maybe Exp) = Path_Pair (Path_Guard (Maybe Exp))
                                                    (Path_Exp (Maybe Exp))
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Maybe Exp for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) (Maybe Inline)
        where type PathType ((Guard, Exp))
                            (Maybe Inline) = Path_Pair (Path_Guard (Maybe Inline))
                                                       (Path_Exp (Maybe Inline))
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Maybe Inline for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) (Maybe Type)
        where type PathType ((Guard, Exp))
                            (Maybe Type) = Path_Pair (Path_Guard (Maybe Type))
                                                     (Path_Exp (Maybe Type))
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Maybe Type for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) ([(Name, Strict, Type)])
        where type PathType ((Guard, Exp))
                            ([(Name, Strict, Type)]) = Path_Pair (Path_Guard ([(Name,
                                                                                Strict,
                                                                                Type)]))
                                                                 (Path_Exp ([(Name, Strict, Type)]))
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [(Name, Strict, Type)] for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) ([(Guard, Exp)])
        where type PathType ((Guard, Exp))
                            ([(Guard, Exp)]) = Path_Pair (Path_Guard ([(Guard, Exp)]))
                                                         (Path_Exp ([(Guard, Exp)]))
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [(Guard, Exp)] for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) ([(Name, Exp)])
        where type PathType ((Guard, Exp))
                            ([(Name, Exp)]) = Path_Pair (Path_Guard ([(Name, Exp)]))
                                                        (Path_Exp ([(Name, Exp)]))
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [(Name, Exp)] for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) ([(Name, Pat)])
        where type PathType ((Guard, Exp))
                            ([(Name, Pat)]) = Path_Pair (Path_Guard ([(Name, Pat)]))
                                                        (Path_Exp ([(Name, Pat)]))
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [(Name, Pat)] for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) ([(Strict, Type)])
        where type PathType ((Guard, Exp))
                            ([(Strict, Type)]) = Path_Pair (Path_Guard ([(Strict, Type)]))
                                                           (Path_Exp ([(Strict, Type)]))
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [(Strict, Type)] for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) ([[Stmt]])
        where type PathType ((Guard, Exp))
                            ([[Stmt]]) = Path_Pair (Path_Guard ([[Stmt]]))
                                                   (Path_Exp ([[Stmt]]))
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [[Stmt]] for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) ([Word8])
        where type PathType ((Guard, Exp))
                            ([Word8]) = Path_Pair (Path_Guard ([Word8])) (Path_Exp ([Word8]))
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Word8] for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) ([Clause])
        where type PathType ((Guard, Exp))
                            ([Clause]) = Path_Pair (Path_Guard ([Clause]))
                                                   (Path_Exp ([Clause]))
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Clause] for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) ([Con])
        where type PathType ((Guard, Exp))
                            ([Con]) = Path_Pair (Path_Guard ([Con])) (Path_Exp ([Con]))
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Con] for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) ([Dec])
        where type PathType ((Guard, Exp))
                            ([Dec]) = Path_Pair (Path_Guard ([Dec])) (Path_Exp ([Dec]))
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Dec] for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) ([Exp])
        where type PathType ((Guard, Exp))
                            ([Exp]) = Path_Pair (Path_Guard ([Exp])) (Path_Exp ([Exp]))
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Exp] for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) ([FunDep])
        where type PathType ((Guard, Exp))
                            ([FunDep]) = Path_Pair (Path_Guard ([FunDep]))
                                                   (Path_Exp ([FunDep]))
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [FunDep] for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) ([Match])
        where type PathType ((Guard, Exp))
                            ([Match]) = Path_Pair (Path_Guard ([Match])) (Path_Exp ([Match]))
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Match] for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) ([Name])
        where type PathType ((Guard, Exp))
                            ([Name]) = Path_Pair (Path_Guard ([Name])) (Path_Exp ([Name]))
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Name] for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) ([Pat])
        where type PathType ((Guard, Exp))
                            ([Pat]) = Path_Pair (Path_Guard ([Pat])) (Path_Exp ([Pat]))
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Pat] for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) ([Role])
        where type PathType ((Guard, Exp))
                            ([Role]) = Path_Pair (Path_Guard ([Role])) (Path_Exp ([Role]))
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Role] for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) ([RuleBndr])
        where type PathType ((Guard, Exp))
                            ([RuleBndr]) = Path_Pair (Path_Guard ([RuleBndr]))
                                                     (Path_Exp ([RuleBndr]))
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [RuleBndr] for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) ([Stmt])
        where type PathType ((Guard, Exp))
                            ([Stmt]) = Path_Pair (Path_Guard ([Stmt])) (Path_Exp ([Stmt]))
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Stmt] for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) ([TySynEqn])
        where type PathType ((Guard, Exp))
                            ([TySynEqn]) = Path_Pair (Path_Guard ([TySynEqn]))
                                                     (Path_Exp ([TySynEqn]))
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [TySynEqn] for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) ([TyVarBndr])
        where type PathType ((Guard, Exp))
                            ([TyVarBndr]) = Path_Pair (Path_Guard ([TyVarBndr]))
                                                      (Path_Exp ([TyVarBndr]))
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [TyVarBndr] for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Word8
        where type PathType ((Guard, Exp))
                            Word8 = Path_Pair (Path_Guard Word8) (Path_Exp Word8)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Word8 for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Char
        where type PathType ((Guard, Exp))
                            Char = Path_Pair (Path_Guard Char) (Path_Exp Char)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Char for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Int
        where type PathType ((Guard, Exp)) Int = Path_Pair (Path_Guard Int)
                                                           (Path_Exp Int)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Int for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) BigNat
        where type PathType ((Guard, Exp))
                            BigNat = Path_Pair (Path_Guard BigNat) (Path_Exp BigNat)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal BigNat for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Integer
        where type PathType ((Guard, Exp))
                            Integer = Path_Pair (Path_Guard Integer) (Path_Exp Integer)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Integer for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) AnnTarget
        where type PathType ((Guard, Exp))
                            AnnTarget = Path_Pair (Path_Guard AnnTarget) (Path_Exp AnnTarget)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal AnnTarget for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Body
        where type PathType ((Guard, Exp))
                            Body = Path_Pair (Path_Guard Body) (Path_Exp Body)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Body for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Callconv
        where type PathType ((Guard, Exp))
                            Callconv = Path_Pair (Path_Guard Callconv) (Path_Exp Callconv)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Callconv for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Clause
        where type PathType ((Guard, Exp))
                            Clause = Path_Pair (Path_Guard Clause) (Path_Exp Clause)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Clause for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Con
        where type PathType ((Guard, Exp)) Con = Path_Pair (Path_Guard Con)
                                                           (Path_Exp Con)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Con for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Dec
        where type PathType ((Guard, Exp)) Dec = Path_Pair (Path_Guard Dec)
                                                           (Path_Exp Dec)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Dec for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Exp
        where type PathType ((Guard, Exp)) Exp = Path_Pair (Path_Guard Exp)
                                                           (Path_Exp Exp)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second _) = _2
              toLens u = error $ ("Unexpected goal Exp for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) FamFlavour
        where type PathType ((Guard, Exp))
                            FamFlavour = Path_Pair (Path_Guard FamFlavour)
                                                   (Path_Exp FamFlavour)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal FamFlavour for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Fixity
        where type PathType ((Guard, Exp))
                            Fixity = Path_Pair (Path_Guard Fixity) (Path_Exp Fixity)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Fixity for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) FixityDirection
        where type PathType ((Guard, Exp))
                            FixityDirection = Path_Pair (Path_Guard FixityDirection)
                                                        (Path_Exp FixityDirection)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal FixityDirection for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Foreign
        where type PathType ((Guard, Exp))
                            Foreign = Path_Pair (Path_Guard Foreign) (Path_Exp Foreign)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Foreign for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) FunDep
        where type PathType ((Guard, Exp))
                            FunDep = Path_Pair (Path_Guard FunDep) (Path_Exp FunDep)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal FunDep for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Guard
        where type PathType ((Guard, Exp))
                            Guard = Path_Pair (Path_Guard Guard) (Path_Exp Guard)
              toLens (Path_First _) = _1
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Guard for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Inline
        where type PathType ((Guard, Exp))
                            Inline = Path_Pair (Path_Guard Inline) (Path_Exp Inline)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Inline for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Lit
        where type PathType ((Guard, Exp)) Lit = Path_Pair (Path_Guard Lit)
                                                           (Path_Exp Lit)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Lit for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Match
        where type PathType ((Guard, Exp))
                            Match = Path_Pair (Path_Guard Match) (Path_Exp Match)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Match for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) ModName
        where type PathType ((Guard, Exp))
                            ModName = Path_Pair (Path_Guard ModName) (Path_Exp ModName)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal ModName for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Name
        where type PathType ((Guard, Exp))
                            Name = Path_Pair (Path_Guard Name) (Path_Exp Name)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Name for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) NameFlavour
        where type PathType ((Guard, Exp))
                            NameFlavour = Path_Pair (Path_Guard NameFlavour)
                                                    (Path_Exp NameFlavour)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal NameFlavour for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) NameSpace
        where type PathType ((Guard, Exp))
                            NameSpace = Path_Pair (Path_Guard NameSpace) (Path_Exp NameSpace)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal NameSpace for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) OccName
        where type PathType ((Guard, Exp))
                            OccName = Path_Pair (Path_Guard OccName) (Path_Exp OccName)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal OccName for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Pat
        where type PathType ((Guard, Exp)) Pat = Path_Pair (Path_Guard Pat)
                                                           (Path_Exp Pat)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Pat for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Phases
        where type PathType ((Guard, Exp))
                            Phases = Path_Pair (Path_Guard Phases) (Path_Exp Phases)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Phases for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) PkgName
        where type PathType ((Guard, Exp))
                            PkgName = Path_Pair (Path_Guard PkgName) (Path_Exp PkgName)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal PkgName for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Pragma
        where type PathType ((Guard, Exp))
                            Pragma = Path_Pair (Path_Guard Pragma) (Path_Exp Pragma)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Pragma for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Range
        where type PathType ((Guard, Exp))
                            Range = Path_Pair (Path_Guard Range) (Path_Exp Range)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Range for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Role
        where type PathType ((Guard, Exp))
                            Role = Path_Pair (Path_Guard Role) (Path_Exp Role)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Role for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) RuleBndr
        where type PathType ((Guard, Exp))
                            RuleBndr = Path_Pair (Path_Guard RuleBndr) (Path_Exp RuleBndr)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal RuleBndr for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) RuleMatch
        where type PathType ((Guard, Exp))
                            RuleMatch = Path_Pair (Path_Guard RuleMatch) (Path_Exp RuleMatch)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal RuleMatch for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Safety
        where type PathType ((Guard, Exp))
                            Safety = Path_Pair (Path_Guard Safety) (Path_Exp Safety)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Safety for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Stmt
        where type PathType ((Guard, Exp))
                            Stmt = Path_Pair (Path_Guard Stmt) (Path_Exp Stmt)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Stmt for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Strict
        where type PathType ((Guard, Exp))
                            Strict = Path_Pair (Path_Guard Strict) (Path_Exp Strict)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Strict for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) TyLit
        where type PathType ((Guard, Exp))
                            TyLit = Path_Pair (Path_Guard TyLit) (Path_Exp TyLit)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal TyLit for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) TySynEqn
        where type PathType ((Guard, Exp))
                            TySynEqn = Path_Pair (Path_Guard TySynEqn) (Path_Exp TySynEqn)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal TySynEqn for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) TyVarBndr
        where type PathType ((Guard, Exp))
                            TyVarBndr = Path_Pair (Path_Guard TyVarBndr) (Path_Exp TyVarBndr)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal TyVarBndr for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) String
        where type PathType ((Guard, Exp))
                            String = Path_Pair (Path_Guard String) (Path_Exp String)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Char] (aka String) for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Rational
        where type PathType ((Guard, Exp))
                            Rational = Path_Pair (Path_Guard Rational) (Path_Exp Rational)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Ratio Integer (aka Rational) for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Cxt
        where type PathType ((Guard, Exp)) Cxt = Path_Pair (Path_Guard Cxt)
                                                           (Path_Exp Cxt)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Type] (aka Cxt) for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) FieldExp
        where type PathType ((Guard, Exp))
                            FieldExp = Path_Pair (Path_Guard FieldExp) (Path_Exp FieldExp)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal (Name, Exp) (aka FieldExp) for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) FieldPat
        where type PathType ((Guard, Exp))
                            FieldPat = Path_Pair (Path_Guard FieldPat) (Path_Exp FieldPat)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal (Name, Pat) (aka FieldPat) for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) Type
        where type PathType ((Guard, Exp))
                            Type = Path_Pair (Path_Guard Type) (Path_Exp Type)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Type (aka Kind, aka Pred) for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) StrictType
        where type PathType ((Guard, Exp))
                            StrictType = Path_Pair (Path_Guard StrictType)
                                                   (Path_Exp StrictType)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal (Strict, Type) (aka StrictType) for (Guard, Exp): " ++ show u)
    instance Path ((Guard, Exp)) VarStrictType
        where type PathType ((Guard, Exp))
                            VarStrictType = Path_Pair (Path_Guard VarStrictType)
                                                      (Path_Exp VarStrictType)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal (Name, Strict, Type) (aka VarStrictType) for (Guard, Exp): " ++ show u)
    instance Path (Maybe Exp) ((Guard, Exp))
        where type PathType (Maybe Exp)
                            ((Guard, Exp)) = Path_Maybe (Path_Exp ((Guard, Exp)))
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal (Guard, Exp) for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) (Maybe Exp)
        where type PathType (Maybe Exp)
                            (Maybe Exp) = Path_Maybe (Path_Exp (Maybe Exp))
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Maybe Exp for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) (Maybe Inline)
        where type PathType (Maybe Exp)
                            (Maybe Inline) = Path_Maybe (Path_Exp (Maybe Inline))
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Maybe Inline for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) (Maybe Type)
        where type PathType (Maybe Exp)
                            (Maybe Type) = Path_Maybe (Path_Exp (Maybe Type))
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Maybe Type for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) ([(Name, Strict, Type)])
        where type PathType (Maybe Exp)
                            ([(Name, Strict, Type)]) = Path_Maybe (Path_Exp ([(Name,
                                                                               Strict,
                                                                               Type)]))
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal [(Name, Strict, Type)] for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) ([(Guard, Exp)])
        where type PathType (Maybe Exp)
                            ([(Guard, Exp)]) = Path_Maybe (Path_Exp ([(Guard, Exp)]))
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal [(Guard, Exp)] for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) ([(Name, Exp)])
        where type PathType (Maybe Exp)
                            ([(Name, Exp)]) = Path_Maybe (Path_Exp ([(Name, Exp)]))
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal [(Name, Exp)] for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) ([(Name, Pat)])
        where type PathType (Maybe Exp)
                            ([(Name, Pat)]) = Path_Maybe (Path_Exp ([(Name, Pat)]))
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal [(Name, Pat)] for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) ([(Strict, Type)])
        where type PathType (Maybe Exp)
                            ([(Strict, Type)]) = Path_Maybe (Path_Exp ([(Strict, Type)]))
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal [(Strict, Type)] for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) ([[Stmt]])
        where type PathType (Maybe Exp)
                            ([[Stmt]]) = Path_Maybe (Path_Exp ([[Stmt]]))
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal [[Stmt]] for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) ([Word8])
        where type PathType (Maybe Exp)
                            ([Word8]) = Path_Maybe (Path_Exp ([Word8]))
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal [Word8] for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) ([Clause])
        where type PathType (Maybe Exp)
                            ([Clause]) = Path_Maybe (Path_Exp ([Clause]))
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal [Clause] for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) ([Con])
        where type PathType (Maybe Exp)
                            ([Con]) = Path_Maybe (Path_Exp ([Con]))
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal [Con] for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) ([Dec])
        where type PathType (Maybe Exp)
                            ([Dec]) = Path_Maybe (Path_Exp ([Dec]))
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal [Dec] for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) ([Exp])
        where type PathType (Maybe Exp)
                            ([Exp]) = Path_Maybe (Path_Exp ([Exp]))
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal [Exp] for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) ([FunDep])
        where type PathType (Maybe Exp)
                            ([FunDep]) = Path_Maybe (Path_Exp ([FunDep]))
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal [FunDep] for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) ([Match])
        where type PathType (Maybe Exp)
                            ([Match]) = Path_Maybe (Path_Exp ([Match]))
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal [Match] for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) ([Name])
        where type PathType (Maybe Exp)
                            ([Name]) = Path_Maybe (Path_Exp ([Name]))
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal [Name] for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) ([Pat])
        where type PathType (Maybe Exp)
                            ([Pat]) = Path_Maybe (Path_Exp ([Pat]))
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal [Pat] for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) ([Role])
        where type PathType (Maybe Exp)
                            ([Role]) = Path_Maybe (Path_Exp ([Role]))
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal [Role] for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) ([RuleBndr])
        where type PathType (Maybe Exp)
                            ([RuleBndr]) = Path_Maybe (Path_Exp ([RuleBndr]))
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal [RuleBndr] for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) ([Stmt])
        where type PathType (Maybe Exp)
                            ([Stmt]) = Path_Maybe (Path_Exp ([Stmt]))
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal [Stmt] for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) ([TySynEqn])
        where type PathType (Maybe Exp)
                            ([TySynEqn]) = Path_Maybe (Path_Exp ([TySynEqn]))
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal [TySynEqn] for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) ([TyVarBndr])
        where type PathType (Maybe Exp)
                            ([TyVarBndr]) = Path_Maybe (Path_Exp ([TyVarBndr]))
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal [TyVarBndr] for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Word8
        where type PathType (Maybe Exp) Word8 = Path_Maybe (Path_Exp Word8)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Word8 for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Char
        where type PathType (Maybe Exp) Char = Path_Maybe (Path_Exp Char)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Char for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Int
        where type PathType (Maybe Exp) Int = Path_Maybe (Path_Exp Int)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Int for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) BigNat
        where type PathType (Maybe Exp)
                            BigNat = Path_Maybe (Path_Exp BigNat)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal BigNat for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Integer
        where type PathType (Maybe Exp)
                            Integer = Path_Maybe (Path_Exp Integer)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Integer for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) AnnTarget
        where type PathType (Maybe Exp)
                            AnnTarget = Path_Maybe (Path_Exp AnnTarget)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal AnnTarget for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Body
        where type PathType (Maybe Exp) Body = Path_Maybe (Path_Exp Body)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Body for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Callconv
        where type PathType (Maybe Exp)
                            Callconv = Path_Maybe (Path_Exp Callconv)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Callconv for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Clause
        where type PathType (Maybe Exp)
                            Clause = Path_Maybe (Path_Exp Clause)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Clause for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Con
        where type PathType (Maybe Exp) Con = Path_Maybe (Path_Exp Con)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Con for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Dec
        where type PathType (Maybe Exp) Dec = Path_Maybe (Path_Exp Dec)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Dec for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Exp
        where type PathType (Maybe Exp) Exp = Path_Maybe (Path_Exp Exp)
              toLens (Path_Just _) = _Just
              toLens u = error $ ("Unexpected goal Exp for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) FamFlavour
        where type PathType (Maybe Exp)
                            FamFlavour = Path_Maybe (Path_Exp FamFlavour)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal FamFlavour for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Fixity
        where type PathType (Maybe Exp)
                            Fixity = Path_Maybe (Path_Exp Fixity)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Fixity for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) FixityDirection
        where type PathType (Maybe Exp)
                            FixityDirection = Path_Maybe (Path_Exp FixityDirection)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal FixityDirection for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Foreign
        where type PathType (Maybe Exp)
                            Foreign = Path_Maybe (Path_Exp Foreign)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Foreign for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) FunDep
        where type PathType (Maybe Exp)
                            FunDep = Path_Maybe (Path_Exp FunDep)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal FunDep for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Guard
        where type PathType (Maybe Exp) Guard = Path_Maybe (Path_Exp Guard)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Guard for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Inline
        where type PathType (Maybe Exp)
                            Inline = Path_Maybe (Path_Exp Inline)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Inline for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Lit
        where type PathType (Maybe Exp) Lit = Path_Maybe (Path_Exp Lit)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Lit for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Match
        where type PathType (Maybe Exp) Match = Path_Maybe (Path_Exp Match)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Match for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) ModName
        where type PathType (Maybe Exp)
                            ModName = Path_Maybe (Path_Exp ModName)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal ModName for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Name
        where type PathType (Maybe Exp) Name = Path_Maybe (Path_Exp Name)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Name for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) NameFlavour
        where type PathType (Maybe Exp)
                            NameFlavour = Path_Maybe (Path_Exp NameFlavour)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal NameFlavour for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) NameSpace
        where type PathType (Maybe Exp)
                            NameSpace = Path_Maybe (Path_Exp NameSpace)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal NameSpace for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) OccName
        where type PathType (Maybe Exp)
                            OccName = Path_Maybe (Path_Exp OccName)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal OccName for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Pat
        where type PathType (Maybe Exp) Pat = Path_Maybe (Path_Exp Pat)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Pat for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Phases
        where type PathType (Maybe Exp)
                            Phases = Path_Maybe (Path_Exp Phases)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Phases for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) PkgName
        where type PathType (Maybe Exp)
                            PkgName = Path_Maybe (Path_Exp PkgName)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal PkgName for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Pragma
        where type PathType (Maybe Exp)
                            Pragma = Path_Maybe (Path_Exp Pragma)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Pragma for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Range
        where type PathType (Maybe Exp) Range = Path_Maybe (Path_Exp Range)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Range for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Role
        where type PathType (Maybe Exp) Role = Path_Maybe (Path_Exp Role)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Role for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) RuleBndr
        where type PathType (Maybe Exp)
                            RuleBndr = Path_Maybe (Path_Exp RuleBndr)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal RuleBndr for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) RuleMatch
        where type PathType (Maybe Exp)
                            RuleMatch = Path_Maybe (Path_Exp RuleMatch)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal RuleMatch for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Safety
        where type PathType (Maybe Exp)
                            Safety = Path_Maybe (Path_Exp Safety)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Safety for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Stmt
        where type PathType (Maybe Exp) Stmt = Path_Maybe (Path_Exp Stmt)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Stmt for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Strict
        where type PathType (Maybe Exp)
                            Strict = Path_Maybe (Path_Exp Strict)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Strict for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) TyLit
        where type PathType (Maybe Exp) TyLit = Path_Maybe (Path_Exp TyLit)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal TyLit for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) TySynEqn
        where type PathType (Maybe Exp)
                            TySynEqn = Path_Maybe (Path_Exp TySynEqn)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal TySynEqn for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) TyVarBndr
        where type PathType (Maybe Exp)
                            TyVarBndr = Path_Maybe (Path_Exp TyVarBndr)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal TyVarBndr for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) String
        where type PathType (Maybe Exp)
                            String = Path_Maybe (Path_Exp String)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal [Char] (aka String) for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Rational
        where type PathType (Maybe Exp)
                            Rational = Path_Maybe (Path_Exp Rational)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Ratio Integer (aka Rational) for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Cxt
        where type PathType (Maybe Exp) Cxt = Path_Maybe (Path_Exp Cxt)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal [Type] (aka Cxt) for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) FieldExp
        where type PathType (Maybe Exp)
                            FieldExp = Path_Maybe (Path_Exp FieldExp)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal (Name, Exp) (aka FieldExp) for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) FieldPat
        where type PathType (Maybe Exp)
                            FieldPat = Path_Maybe (Path_Exp FieldPat)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal (Name, Pat) (aka FieldPat) for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) Type
        where type PathType (Maybe Exp) Type = Path_Maybe (Path_Exp Type)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Type (aka Kind, aka Pred) for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) StrictType
        where type PathType (Maybe Exp)
                            StrictType = Path_Maybe (Path_Exp StrictType)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal (Strict, Type) (aka StrictType) for Maybe Exp: " ++ show u)
    instance Path (Maybe Exp) VarStrictType
        where type PathType (Maybe Exp)
                            VarStrictType = Path_Maybe (Path_Exp VarStrictType)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal (Name, Strict, Type) (aka VarStrictType) for Maybe Exp: " ++ show u)
    instance Path (Maybe Inline) (Maybe Inline)
        where type PathType (Maybe Inline)
                            (Maybe Inline) = Path_Maybe (Path_Inline (Maybe Inline))
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Maybe Inline for Maybe Inline: " ++ show u)
    instance Path (Maybe Inline) Inline
        where type PathType (Maybe Inline)
                            Inline = Path_Maybe (Path_Inline Inline)
              toLens (Path_Just _) = _Just
              toLens u = error $ ("Unexpected goal Inline for Maybe Inline: " ++ show u)
    instance Path (Maybe Type) (Maybe Type)
        where type PathType (Maybe Type)
                            (Maybe Type) = Path_Maybe (Path_Type (Maybe Type))
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Maybe Type for Maybe Type: " ++ show u)
    instance Path (Maybe Type) ([TyVarBndr])
        where type PathType (Maybe Type)
                            ([TyVarBndr]) = Path_Maybe (Path_Type ([TyVarBndr]))
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal [TyVarBndr] for Maybe Type: " ++ show u)
    instance Path (Maybe Type) Int
        where type PathType (Maybe Type) Int = Path_Maybe (Path_Type Int)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Int for Maybe Type: " ++ show u)
    instance Path (Maybe Type) BigNat
        where type PathType (Maybe Type)
                            BigNat = Path_Maybe (Path_Type BigNat)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal BigNat for Maybe Type: " ++ show u)
    instance Path (Maybe Type) Integer
        where type PathType (Maybe Type)
                            Integer = Path_Maybe (Path_Type Integer)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Integer for Maybe Type: " ++ show u)
    instance Path (Maybe Type) ModName
        where type PathType (Maybe Type)
                            ModName = Path_Maybe (Path_Type ModName)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal ModName for Maybe Type: " ++ show u)
    instance Path (Maybe Type) Name
        where type PathType (Maybe Type) Name = Path_Maybe (Path_Type Name)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal Name for Maybe Type: " ++ show u)
    instance Path (Maybe Type) NameFlavour
        where type PathType (Maybe Type)
                            NameFlavour = Path_Maybe (Path_Type NameFlavour)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal NameFlavour for Maybe Type: " ++ show u)
    instance Path (Maybe Type) NameSpace
        where type PathType (Maybe Type)
                            NameSpace = Path_Maybe (Path_Type NameSpace)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal NameSpace for Maybe Type: " ++ show u)
    instance Path (Maybe Type) OccName
        where type PathType (Maybe Type)
                            OccName = Path_Maybe (Path_Type OccName)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal OccName for Maybe Type: " ++ show u)
    instance Path (Maybe Type) PkgName
        where type PathType (Maybe Type)
                            PkgName = Path_Maybe (Path_Type PkgName)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal PkgName for Maybe Type: " ++ show u)
    instance Path (Maybe Type) TyLit
        where type PathType (Maybe Type)
                            TyLit = Path_Maybe (Path_Type TyLit)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal TyLit for Maybe Type: " ++ show u)
    instance Path (Maybe Type) TyVarBndr
        where type PathType (Maybe Type)
                            TyVarBndr = Path_Maybe (Path_Type TyVarBndr)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal TyVarBndr for Maybe Type: " ++ show u)
    instance Path (Maybe Type) String
        where type PathType (Maybe Type)
                            String = Path_Maybe (Path_Type String)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal [Char] (aka String) for Maybe Type: " ++ show u)
    instance Path (Maybe Type) Cxt
        where type PathType (Maybe Type) Cxt = Path_Maybe (Path_Type Cxt)
              toLens (Path_Just v) = _Just . toLens v
              toLens u = error $ ("Unexpected goal [Type] (aka Cxt) for Maybe Type: " ++ show u)
    instance Path (Maybe Type) Type
        where type PathType (Maybe Type) Type = Path_Maybe (Path_Type Type)
              toLens (Path_Just _) = _Just
              toLens u = error $ ("Unexpected goal Type (aka Kind, aka Pred) for Maybe Type: " ++ show u)
    instance Path ([(Name, Strict, Type)]) ([(Name, Strict, Type)])
        where type PathType ([(Name, Strict, Type)])
                            ([(Name, Strict, Type)]) = Path_List (Path_VarStrictType ([(Name,
                                                                                        Strict,
                                                                                        Type)]))
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal [(Name, Strict, Type)] for [(Name, Strict, Type)]: " ++ show u)
    instance Path ([(Guard, Exp)]) ([(Guard, Exp)])
        where type PathType ([(Guard, Exp)])
                            ([(Guard, Exp)]) = Path_List (Path_Pair (Path_Guard ([(Guard,
                                                                                   Exp)]))
                                                                    (Path_Exp ([(Guard, Exp)])))
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal [(Guard, Exp)] for [(Guard, Exp)]: " ++ show u)
    instance Path ([(Name, Exp)]) ([(Name, Exp)])
        where type PathType ([(Name, Exp)])
                            ([(Name, Exp)]) = Path_List (Path_Pair (Path_Name ([(Name, Exp)]))
                                                                   (Path_Exp ([(Name, Exp)])))
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal [(Name, Exp)] for [(Name, Exp)]: " ++ show u)
    instance Path ([(Name, Pat)]) ([(Name, Pat)])
        where type PathType ([(Name, Pat)])
                            ([(Name, Pat)]) = Path_List (Path_Pair (Path_Name ([(Name, Pat)]))
                                                                   (Path_Pat ([(Name, Pat)])))
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal [(Name, Pat)] for [(Name, Pat)]: " ++ show u)
    instance Path ([(Strict, Type)]) ([(Strict, Type)])
        where type PathType ([(Strict, Type)])
                            ([(Strict, Type)]) = Path_List (Path_Pair (Path_Strict ([(Strict,
                                                                                      Type)]))
                                                                      (Path_Type ([(Strict, Type)])))
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal [(Strict, Type)] for [(Strict, Type)]: " ++ show u)
    instance Path ([[Stmt]]) ([[Stmt]])
        where type PathType ([[Stmt]])
                            ([[Stmt]]) = Path_List (Path_List (Path_Stmt ([[Stmt]])))
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal [[Stmt]] for [[Stmt]]: " ++ show u)
    instance Path ([Word8]) ([Word8])
        where type PathType ([Word8])
                            ([Word8]) = Path_List (Path_Word8 ([Word8]))
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal [Word8] for [Word8]: " ++ show u)
    instance Path ([Clause]) ([Clause])
        where type PathType ([Clause])
                            ([Clause]) = Path_List (Path_Clause ([Clause]))
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal [Clause] for [Clause]: " ++ show u)
    instance Path ([Con]) ([Con])
        where type PathType ([Con]) ([Con]) = Path_List (Path_Con ([Con]))
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal [Con] for [Con]: " ++ show u)
    instance Path ([Dec]) ([Dec])
        where type PathType ([Dec]) ([Dec]) = Path_List (Path_Dec ([Dec]))
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal [Dec] for [Dec]: " ++ show u)
    instance Path ([Exp]) ([Exp])
        where type PathType ([Exp]) ([Exp]) = Path_List (Path_Exp ([Exp]))
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal [Exp] for [Exp]: " ++ show u)
    instance Path ([FunDep]) ([FunDep])
        where type PathType ([FunDep])
                            ([FunDep]) = Path_List (Path_FunDep ([FunDep]))
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal [FunDep] for [FunDep]: " ++ show u)
    instance Path ([Match]) ([Match])
        where type PathType ([Match])
                            ([Match]) = Path_List (Path_Match ([Match]))
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal [Match] for [Match]: " ++ show u)
    instance Path ([Name]) ([Name])
        where type PathType ([Name])
                            ([Name]) = Path_List (Path_Name ([Name]))
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal [Name] for [Name]: " ++ show u)
    instance Path ([Pat]) ([Pat])
        where type PathType ([Pat]) ([Pat]) = Path_List (Path_Pat ([Pat]))
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal [Pat] for [Pat]: " ++ show u)
    instance Path ([Role]) ([Role])
        where type PathType ([Role])
                            ([Role]) = Path_List (Path_Role ([Role]))
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal [Role] for [Role]: " ++ show u)
    instance Path ([RuleBndr]) ([RuleBndr])
        where type PathType ([RuleBndr])
                            ([RuleBndr]) = Path_List (Path_RuleBndr ([RuleBndr]))
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal [RuleBndr] for [RuleBndr]: " ++ show u)
    instance Path ([Stmt]) ([Stmt])
        where type PathType ([Stmt])
                            ([Stmt]) = Path_List (Path_Stmt ([Stmt]))
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal [Stmt] for [Stmt]: " ++ show u)
    instance Path ([TySynEqn]) ([TySynEqn])
        where type PathType ([TySynEqn])
                            ([TySynEqn]) = Path_List (Path_TySynEqn ([TySynEqn]))
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal [TySynEqn] for [TySynEqn]: " ++ show u)
    instance Path ([TyVarBndr]) ([TyVarBndr])
        where type PathType ([TyVarBndr])
                            ([TyVarBndr]) = Path_List (Path_TyVarBndr ([TyVarBndr]))
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal [TyVarBndr] for [TyVarBndr]: " ++ show u)
    instance Path Word8 Word8
        where type PathType Word8 Word8 = Path_Word8 Word8
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Word8 for Word8: " ++ show u)
    instance Path Char Char
        where type PathType Char Char = Path_Char Char
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Char for Char: " ++ show u)
    instance Path Int Int
        where type PathType Int Int = Path_Int Int
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Int for Int: " ++ show u)
    instance Path BigNat BigNat
        where type PathType BigNat BigNat = Path_BigNat BigNat
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal BigNat for BigNat: " ++ show u)
    instance Path Integer Integer
        where type PathType Integer Integer = Path_Integer Integer
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Integer for Integer: " ++ show u)
    instance Path AnnTarget AnnTarget
        where type PathType AnnTarget AnnTarget = Path_AnnTarget AnnTarget
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal AnnTarget for AnnTarget: " ++ show u)
    instance Path Body Body
        where type PathType Body Body = Path_Body Body
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Body for Body: " ++ show u)
    instance Path Callconv Callconv
        where type PathType Callconv Callconv = Path_Callconv Callconv
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Callconv for Callconv: " ++ show u)
    instance Path Clause Clause
        where type PathType Clause Clause = Path_Clause Clause
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Clause for Clause: " ++ show u)
    instance Path Con Con
        where type PathType Con Con = Path_Con Con
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Con for Con: " ++ show u)
    instance Path Dec Dec
        where type PathType Dec Dec = Path_Dec Dec
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Dec for Dec: " ++ show u)
    instance Path Exp Exp
        where type PathType Exp Exp = Path_Exp Exp
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Exp for Exp: " ++ show u)
    instance Path FamFlavour FamFlavour
        where type PathType FamFlavour
                            FamFlavour = Path_FamFlavour FamFlavour
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal FamFlavour for FamFlavour: " ++ show u)
    instance Path Fixity Fixity
        where type PathType Fixity Fixity = Path_Fixity Fixity
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Fixity for Fixity: " ++ show u)
    instance Path FixityDirection FixityDirection
        where type PathType FixityDirection
                            FixityDirection = Path_FixityDirection FixityDirection
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal FixityDirection for FixityDirection: " ++ show u)
    instance Path Foreign Foreign
        where type PathType Foreign Foreign = Path_Foreign Foreign
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Foreign for Foreign: " ++ show u)
    instance Path FunDep FunDep
        where type PathType FunDep FunDep = Path_FunDep FunDep
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal FunDep for FunDep: " ++ show u)
    instance Path Guard Guard
        where type PathType Guard Guard = Path_Guard Guard
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Guard for Guard: " ++ show u)
    instance Path Inline Inline
        where type PathType Inline Inline = Path_Inline Inline
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Inline for Inline: " ++ show u)
    instance Path Lit Lit
        where type PathType Lit Lit = Path_Lit Lit
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Lit for Lit: " ++ show u)
    instance Path Match Match
        where type PathType Match Match = Path_Match Match
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Match for Match: " ++ show u)
    instance Path ModName ModName
        where type PathType ModName ModName = Path_ModName ModName
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal ModName for ModName: " ++ show u)
    instance Path Name Name
        where type PathType Name Name = Path_Name Name
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Name for Name: " ++ show u)
    instance Path NameFlavour NameFlavour
        where type PathType NameFlavour
                            NameFlavour = Path_NameFlavour NameFlavour
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal NameFlavour for NameFlavour: " ++ show u)
    instance Path NameSpace NameSpace
        where type PathType NameSpace NameSpace = Path_NameSpace NameSpace
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal NameSpace for NameSpace: " ++ show u)
    instance Path OccName OccName
        where type PathType OccName OccName = Path_OccName OccName
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal OccName for OccName: " ++ show u)
    instance Path Pat Pat
        where type PathType Pat Pat = Path_Pat Pat
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Pat for Pat: " ++ show u)
    instance Path Phases Phases
        where type PathType Phases Phases = Path_Phases Phases
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Phases for Phases: " ++ show u)
    instance Path PkgName PkgName
        where type PathType PkgName PkgName = Path_PkgName PkgName
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal PkgName for PkgName: " ++ show u)
    instance Path Pragma Pragma
        where type PathType Pragma Pragma = Path_Pragma Pragma
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Pragma for Pragma: " ++ show u)
    instance Path Range Range
        where type PathType Range Range = Path_Range Range
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Range for Range: " ++ show u)
    instance Path Role Role
        where type PathType Role Role = Path_Role Role
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Role for Role: " ++ show u)
    instance Path RuleBndr RuleBndr
        where type PathType RuleBndr RuleBndr = Path_RuleBndr RuleBndr
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal RuleBndr for RuleBndr: " ++ show u)
    instance Path RuleMatch RuleMatch
        where type PathType RuleMatch RuleMatch = Path_RuleMatch RuleMatch
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal RuleMatch for RuleMatch: " ++ show u)
    instance Path Safety Safety
        where type PathType Safety Safety = Path_Safety Safety
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Safety for Safety: " ++ show u)
    instance Path Stmt Stmt
        where type PathType Stmt Stmt = Path_Stmt Stmt
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Stmt for Stmt: " ++ show u)
    instance Path Strict Strict
        where type PathType Strict Strict = Path_Strict Strict
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Strict for Strict: " ++ show u)
    instance Path TyLit TyLit
        where type PathType TyLit TyLit = Path_TyLit TyLit
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal TyLit for TyLit: " ++ show u)
    instance Path TySynEqn TySynEqn
        where type PathType TySynEqn TySynEqn = Path_TySynEqn TySynEqn
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal TySynEqn for TySynEqn: " ++ show u)
    instance Path TyVarBndr TyVarBndr
        where type PathType TyVarBndr TyVarBndr = Path_TyVarBndr TyVarBndr
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal TyVarBndr for TyVarBndr: " ++ show u)
    instance Path String String
        where type PathType String String = Path_String String
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal [Char] (aka String) for [Char] (aka String): " ++ show u)
    instance Path Rational Rational
        where type PathType Rational Rational = Path_Rational Rational
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Ratio Integer (aka Rational) for Ratio Integer (aka Rational): " ++ show u)
    instance Path Cxt Cxt
        where type PathType Cxt Cxt = Path_List (Path_Type Cxt)
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal [Type] (aka Cxt) for [Type] (aka Cxt): " ++ show u)
    instance Path FieldExp ((Guard, Exp))
        where type PathType FieldExp
                            ((Guard, Exp)) = Path_Pair (Path_Name ((Guard, Exp)))
                                                       (Path_Exp ((Guard, Exp)))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal (Guard, Exp) for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp (Maybe Exp)
        where type PathType FieldExp
                            (Maybe Exp) = Path_Pair (Path_Name (Maybe Exp))
                                                    (Path_Exp (Maybe Exp))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Maybe Exp for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp (Maybe Inline)
        where type PathType FieldExp
                            (Maybe Inline) = Path_Pair (Path_Name (Maybe Inline))
                                                       (Path_Exp (Maybe Inline))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Maybe Inline for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp (Maybe Type)
        where type PathType FieldExp
                            (Maybe Type) = Path_Pair (Path_Name (Maybe Type))
                                                     (Path_Exp (Maybe Type))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Maybe Type for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp ([(Name, Strict, Type)])
        where type PathType FieldExp
                            ([(Name, Strict, Type)]) = Path_Pair (Path_Name ([(Name,
                                                                               Strict,
                                                                               Type)]))
                                                                 (Path_Exp ([(Name, Strict, Type)]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [(Name, Strict, Type)] for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp ([(Guard, Exp)])
        where type PathType FieldExp
                            ([(Guard, Exp)]) = Path_Pair (Path_Name ([(Guard, Exp)]))
                                                         (Path_Exp ([(Guard, Exp)]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [(Guard, Exp)] for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp ([(Name, Exp)])
        where type PathType FieldExp
                            ([(Name, Exp)]) = Path_Pair (Path_Name ([(Name, Exp)]))
                                                        (Path_Exp ([(Name, Exp)]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [(Name, Exp)] for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp ([(Name, Pat)])
        where type PathType FieldExp
                            ([(Name, Pat)]) = Path_Pair (Path_Name ([(Name, Pat)]))
                                                        (Path_Exp ([(Name, Pat)]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [(Name, Pat)] for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp ([(Strict, Type)])
        where type PathType FieldExp
                            ([(Strict, Type)]) = Path_Pair (Path_Name ([(Strict, Type)]))
                                                           (Path_Exp ([(Strict, Type)]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [(Strict, Type)] for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp ([[Stmt]])
        where type PathType FieldExp
                            ([[Stmt]]) = Path_Pair (Path_Name ([[Stmt]])) (Path_Exp ([[Stmt]]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [[Stmt]] for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp ([Word8])
        where type PathType FieldExp
                            ([Word8]) = Path_Pair (Path_Name ([Word8])) (Path_Exp ([Word8]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Word8] for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp ([Clause])
        where type PathType FieldExp
                            ([Clause]) = Path_Pair (Path_Name ([Clause])) (Path_Exp ([Clause]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Clause] for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp ([Con])
        where type PathType FieldExp
                            ([Con]) = Path_Pair (Path_Name ([Con])) (Path_Exp ([Con]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Con] for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp ([Dec])
        where type PathType FieldExp
                            ([Dec]) = Path_Pair (Path_Name ([Dec])) (Path_Exp ([Dec]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Dec] for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp ([Exp])
        where type PathType FieldExp
                            ([Exp]) = Path_Pair (Path_Name ([Exp])) (Path_Exp ([Exp]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Exp] for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp ([FunDep])
        where type PathType FieldExp
                            ([FunDep]) = Path_Pair (Path_Name ([FunDep])) (Path_Exp ([FunDep]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [FunDep] for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp ([Match])
        where type PathType FieldExp
                            ([Match]) = Path_Pair (Path_Name ([Match])) (Path_Exp ([Match]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Match] for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp ([Name])
        where type PathType FieldExp
                            ([Name]) = Path_Pair (Path_Name ([Name])) (Path_Exp ([Name]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Name] for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp ([Pat])
        where type PathType FieldExp
                            ([Pat]) = Path_Pair (Path_Name ([Pat])) (Path_Exp ([Pat]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Pat] for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp ([Role])
        where type PathType FieldExp
                            ([Role]) = Path_Pair (Path_Name ([Role])) (Path_Exp ([Role]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Role] for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp ([RuleBndr])
        where type PathType FieldExp
                            ([RuleBndr]) = Path_Pair (Path_Name ([RuleBndr]))
                                                     (Path_Exp ([RuleBndr]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [RuleBndr] for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp ([Stmt])
        where type PathType FieldExp
                            ([Stmt]) = Path_Pair (Path_Name ([Stmt])) (Path_Exp ([Stmt]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Stmt] for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp ([TySynEqn])
        where type PathType FieldExp
                            ([TySynEqn]) = Path_Pair (Path_Name ([TySynEqn]))
                                                     (Path_Exp ([TySynEqn]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [TySynEqn] for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp ([TyVarBndr])
        where type PathType FieldExp
                            ([TyVarBndr]) = Path_Pair (Path_Name ([TyVarBndr]))
                                                      (Path_Exp ([TyVarBndr]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [TyVarBndr] for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Word8
        where type PathType FieldExp Word8 = Path_Pair (Path_Name Word8)
                                                       (Path_Exp Word8)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Word8 for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Char
        where type PathType FieldExp Char = Path_Pair (Path_Name Char)
                                                      (Path_Exp Char)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Char for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Int
        where type PathType FieldExp Int = Path_Pair (Path_Name Int)
                                                     (Path_Exp Int)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Int for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp BigNat
        where type PathType FieldExp BigNat = Path_Pair (Path_Name BigNat)
                                                        (Path_Exp BigNat)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal BigNat for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Integer
        where type PathType FieldExp
                            Integer = Path_Pair (Path_Name Integer) (Path_Exp Integer)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Integer for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp AnnTarget
        where type PathType FieldExp
                            AnnTarget = Path_Pair (Path_Name AnnTarget) (Path_Exp AnnTarget)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal AnnTarget for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Body
        where type PathType FieldExp Body = Path_Pair (Path_Name Body)
                                                      (Path_Exp Body)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Body for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Callconv
        where type PathType FieldExp
                            Callconv = Path_Pair (Path_Name Callconv) (Path_Exp Callconv)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Callconv for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Clause
        where type PathType FieldExp Clause = Path_Pair (Path_Name Clause)
                                                        (Path_Exp Clause)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Clause for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Con
        where type PathType FieldExp Con = Path_Pair (Path_Name Con)
                                                     (Path_Exp Con)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Con for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Dec
        where type PathType FieldExp Dec = Path_Pair (Path_Name Dec)
                                                     (Path_Exp Dec)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Dec for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Exp
        where type PathType FieldExp Exp = Path_Pair (Path_Name Exp)
                                                     (Path_Exp Exp)
              toLens (Path_Second _) = _2
              toLens u = error $ ("Unexpected goal Exp for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp FamFlavour
        where type PathType FieldExp
                            FamFlavour = Path_Pair (Path_Name FamFlavour) (Path_Exp FamFlavour)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal FamFlavour for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Fixity
        where type PathType FieldExp Fixity = Path_Pair (Path_Name Fixity)
                                                        (Path_Exp Fixity)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Fixity for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp FixityDirection
        where type PathType FieldExp
                            FixityDirection = Path_Pair (Path_Name FixityDirection)
                                                        (Path_Exp FixityDirection)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal FixityDirection for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Foreign
        where type PathType FieldExp
                            Foreign = Path_Pair (Path_Name Foreign) (Path_Exp Foreign)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Foreign for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp FunDep
        where type PathType FieldExp FunDep = Path_Pair (Path_Name FunDep)
                                                        (Path_Exp FunDep)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal FunDep for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Guard
        where type PathType FieldExp Guard = Path_Pair (Path_Name Guard)
                                                       (Path_Exp Guard)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Guard for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Inline
        where type PathType FieldExp Inline = Path_Pair (Path_Name Inline)
                                                        (Path_Exp Inline)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Inline for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Lit
        where type PathType FieldExp Lit = Path_Pair (Path_Name Lit)
                                                     (Path_Exp Lit)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Lit for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Match
        where type PathType FieldExp Match = Path_Pair (Path_Name Match)
                                                       (Path_Exp Match)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Match for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp ModName
        where type PathType FieldExp
                            ModName = Path_Pair (Path_Name ModName) (Path_Exp ModName)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal ModName for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Name
        where type PathType FieldExp Name = Path_Pair (Path_Name Name)
                                                      (Path_Exp Name)
              toLens (Path_First _) = _1
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Name for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp NameFlavour
        where type PathType FieldExp
                            NameFlavour = Path_Pair (Path_Name NameFlavour)
                                                    (Path_Exp NameFlavour)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal NameFlavour for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp NameSpace
        where type PathType FieldExp
                            NameSpace = Path_Pair (Path_Name NameSpace) (Path_Exp NameSpace)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal NameSpace for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp OccName
        where type PathType FieldExp
                            OccName = Path_Pair (Path_Name OccName) (Path_Exp OccName)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal OccName for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Pat
        where type PathType FieldExp Pat = Path_Pair (Path_Name Pat)
                                                     (Path_Exp Pat)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Pat for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Phases
        where type PathType FieldExp Phases = Path_Pair (Path_Name Phases)
                                                        (Path_Exp Phases)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Phases for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp PkgName
        where type PathType FieldExp
                            PkgName = Path_Pair (Path_Name PkgName) (Path_Exp PkgName)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal PkgName for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Pragma
        where type PathType FieldExp Pragma = Path_Pair (Path_Name Pragma)
                                                        (Path_Exp Pragma)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Pragma for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Range
        where type PathType FieldExp Range = Path_Pair (Path_Name Range)
                                                       (Path_Exp Range)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Range for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Role
        where type PathType FieldExp Role = Path_Pair (Path_Name Role)
                                                      (Path_Exp Role)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Role for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp RuleBndr
        where type PathType FieldExp
                            RuleBndr = Path_Pair (Path_Name RuleBndr) (Path_Exp RuleBndr)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal RuleBndr for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp RuleMatch
        where type PathType FieldExp
                            RuleMatch = Path_Pair (Path_Name RuleMatch) (Path_Exp RuleMatch)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal RuleMatch for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Safety
        where type PathType FieldExp Safety = Path_Pair (Path_Name Safety)
                                                        (Path_Exp Safety)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Safety for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Stmt
        where type PathType FieldExp Stmt = Path_Pair (Path_Name Stmt)
                                                      (Path_Exp Stmt)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Stmt for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Strict
        where type PathType FieldExp Strict = Path_Pair (Path_Name Strict)
                                                        (Path_Exp Strict)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Strict for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp TyLit
        where type PathType FieldExp TyLit = Path_Pair (Path_Name TyLit)
                                                       (Path_Exp TyLit)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal TyLit for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp TySynEqn
        where type PathType FieldExp
                            TySynEqn = Path_Pair (Path_Name TySynEqn) (Path_Exp TySynEqn)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal TySynEqn for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp TyVarBndr
        where type PathType FieldExp
                            TyVarBndr = Path_Pair (Path_Name TyVarBndr) (Path_Exp TyVarBndr)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal TyVarBndr for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp String
        where type PathType FieldExp String = Path_Pair (Path_Name String)
                                                        (Path_Exp String)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Char] (aka String) for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Rational
        where type PathType FieldExp
                            Rational = Path_Pair (Path_Name Rational) (Path_Exp Rational)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Ratio Integer (aka Rational) for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Cxt
        where type PathType FieldExp Cxt = Path_Pair (Path_Name Cxt)
                                                     (Path_Exp Cxt)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Type] (aka Cxt) for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp FieldExp
        where type PathType FieldExp
                            FieldExp = Path_Pair (Path_Name FieldExp) (Path_Exp FieldExp)
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal (Name, Exp) (aka FieldExp) for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp FieldPat
        where type PathType FieldExp
                            FieldPat = Path_Pair (Path_Name FieldPat) (Path_Exp FieldPat)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal (Name, Pat) (aka FieldPat) for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp Type
        where type PathType FieldExp Type = Path_Pair (Path_Name Type)
                                                      (Path_Exp Type)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Type (aka Kind, aka Pred) for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp StrictType
        where type PathType FieldExp
                            StrictType = Path_Pair (Path_Name StrictType) (Path_Exp StrictType)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal (Strict, Type) (aka StrictType) for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldExp VarStrictType
        where type PathType FieldExp
                            VarStrictType = Path_Pair (Path_Name VarStrictType)
                                                      (Path_Exp VarStrictType)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal (Name, Strict, Type) (aka VarStrictType) for (Name, Exp) (aka FieldExp): " ++ show u)
    instance Path FieldPat ((Guard, Exp))
        where type PathType FieldPat
                            ((Guard, Exp)) = Path_Pair (Path_Name ((Guard, Exp)))
                                                       (Path_Pat ((Guard, Exp)))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal (Guard, Exp) for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat (Maybe Exp)
        where type PathType FieldPat
                            (Maybe Exp) = Path_Pair (Path_Name (Maybe Exp))
                                                    (Path_Pat (Maybe Exp))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Maybe Exp for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat (Maybe Inline)
        where type PathType FieldPat
                            (Maybe Inline) = Path_Pair (Path_Name (Maybe Inline))
                                                       (Path_Pat (Maybe Inline))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Maybe Inline for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat (Maybe Type)
        where type PathType FieldPat
                            (Maybe Type) = Path_Pair (Path_Name (Maybe Type))
                                                     (Path_Pat (Maybe Type))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Maybe Type for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat ([(Name, Strict, Type)])
        where type PathType FieldPat
                            ([(Name, Strict, Type)]) = Path_Pair (Path_Name ([(Name,
                                                                               Strict,
                                                                               Type)]))
                                                                 (Path_Pat ([(Name, Strict, Type)]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [(Name, Strict, Type)] for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat ([(Guard, Exp)])
        where type PathType FieldPat
                            ([(Guard, Exp)]) = Path_Pair (Path_Name ([(Guard, Exp)]))
                                                         (Path_Pat ([(Guard, Exp)]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [(Guard, Exp)] for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat ([(Name, Exp)])
        where type PathType FieldPat
                            ([(Name, Exp)]) = Path_Pair (Path_Name ([(Name, Exp)]))
                                                        (Path_Pat ([(Name, Exp)]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [(Name, Exp)] for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat ([(Name, Pat)])
        where type PathType FieldPat
                            ([(Name, Pat)]) = Path_Pair (Path_Name ([(Name, Pat)]))
                                                        (Path_Pat ([(Name, Pat)]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [(Name, Pat)] for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat ([(Strict, Type)])
        where type PathType FieldPat
                            ([(Strict, Type)]) = Path_Pair (Path_Name ([(Strict, Type)]))
                                                           (Path_Pat ([(Strict, Type)]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [(Strict, Type)] for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat ([[Stmt]])
        where type PathType FieldPat
                            ([[Stmt]]) = Path_Pair (Path_Name ([[Stmt]])) (Path_Pat ([[Stmt]]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [[Stmt]] for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat ([Word8])
        where type PathType FieldPat
                            ([Word8]) = Path_Pair (Path_Name ([Word8])) (Path_Pat ([Word8]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Word8] for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat ([Clause])
        where type PathType FieldPat
                            ([Clause]) = Path_Pair (Path_Name ([Clause])) (Path_Pat ([Clause]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Clause] for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat ([Con])
        where type PathType FieldPat
                            ([Con]) = Path_Pair (Path_Name ([Con])) (Path_Pat ([Con]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Con] for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat ([Dec])
        where type PathType FieldPat
                            ([Dec]) = Path_Pair (Path_Name ([Dec])) (Path_Pat ([Dec]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Dec] for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat ([Exp])
        where type PathType FieldPat
                            ([Exp]) = Path_Pair (Path_Name ([Exp])) (Path_Pat ([Exp]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Exp] for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat ([FunDep])
        where type PathType FieldPat
                            ([FunDep]) = Path_Pair (Path_Name ([FunDep])) (Path_Pat ([FunDep]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [FunDep] for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat ([Match])
        where type PathType FieldPat
                            ([Match]) = Path_Pair (Path_Name ([Match])) (Path_Pat ([Match]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Match] for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat ([Name])
        where type PathType FieldPat
                            ([Name]) = Path_Pair (Path_Name ([Name])) (Path_Pat ([Name]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Name] for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat ([Pat])
        where type PathType FieldPat
                            ([Pat]) = Path_Pair (Path_Name ([Pat])) (Path_Pat ([Pat]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Pat] for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat ([Role])
        where type PathType FieldPat
                            ([Role]) = Path_Pair (Path_Name ([Role])) (Path_Pat ([Role]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Role] for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat ([RuleBndr])
        where type PathType FieldPat
                            ([RuleBndr]) = Path_Pair (Path_Name ([RuleBndr]))
                                                     (Path_Pat ([RuleBndr]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [RuleBndr] for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat ([Stmt])
        where type PathType FieldPat
                            ([Stmt]) = Path_Pair (Path_Name ([Stmt])) (Path_Pat ([Stmt]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Stmt] for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat ([TySynEqn])
        where type PathType FieldPat
                            ([TySynEqn]) = Path_Pair (Path_Name ([TySynEqn]))
                                                     (Path_Pat ([TySynEqn]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [TySynEqn] for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat ([TyVarBndr])
        where type PathType FieldPat
                            ([TyVarBndr]) = Path_Pair (Path_Name ([TyVarBndr]))
                                                      (Path_Pat ([TyVarBndr]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [TyVarBndr] for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Word8
        where type PathType FieldPat Word8 = Path_Pair (Path_Name Word8)
                                                       (Path_Pat Word8)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Word8 for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Char
        where type PathType FieldPat Char = Path_Pair (Path_Name Char)
                                                      (Path_Pat Char)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Char for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Int
        where type PathType FieldPat Int = Path_Pair (Path_Name Int)
                                                     (Path_Pat Int)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Int for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat BigNat
        where type PathType FieldPat BigNat = Path_Pair (Path_Name BigNat)
                                                        (Path_Pat BigNat)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal BigNat for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Integer
        where type PathType FieldPat
                            Integer = Path_Pair (Path_Name Integer) (Path_Pat Integer)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Integer for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat AnnTarget
        where type PathType FieldPat
                            AnnTarget = Path_Pair (Path_Name AnnTarget) (Path_Pat AnnTarget)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal AnnTarget for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Body
        where type PathType FieldPat Body = Path_Pair (Path_Name Body)
                                                      (Path_Pat Body)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Body for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Callconv
        where type PathType FieldPat
                            Callconv = Path_Pair (Path_Name Callconv) (Path_Pat Callconv)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Callconv for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Clause
        where type PathType FieldPat Clause = Path_Pair (Path_Name Clause)
                                                        (Path_Pat Clause)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Clause for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Con
        where type PathType FieldPat Con = Path_Pair (Path_Name Con)
                                                     (Path_Pat Con)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Con for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Dec
        where type PathType FieldPat Dec = Path_Pair (Path_Name Dec)
                                                     (Path_Pat Dec)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Dec for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Exp
        where type PathType FieldPat Exp = Path_Pair (Path_Name Exp)
                                                     (Path_Pat Exp)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Exp for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat FamFlavour
        where type PathType FieldPat
                            FamFlavour = Path_Pair (Path_Name FamFlavour) (Path_Pat FamFlavour)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal FamFlavour for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Fixity
        where type PathType FieldPat Fixity = Path_Pair (Path_Name Fixity)
                                                        (Path_Pat Fixity)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Fixity for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat FixityDirection
        where type PathType FieldPat
                            FixityDirection = Path_Pair (Path_Name FixityDirection)
                                                        (Path_Pat FixityDirection)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal FixityDirection for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Foreign
        where type PathType FieldPat
                            Foreign = Path_Pair (Path_Name Foreign) (Path_Pat Foreign)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Foreign for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat FunDep
        where type PathType FieldPat FunDep = Path_Pair (Path_Name FunDep)
                                                        (Path_Pat FunDep)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal FunDep for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Guard
        where type PathType FieldPat Guard = Path_Pair (Path_Name Guard)
                                                       (Path_Pat Guard)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Guard for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Inline
        where type PathType FieldPat Inline = Path_Pair (Path_Name Inline)
                                                        (Path_Pat Inline)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Inline for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Lit
        where type PathType FieldPat Lit = Path_Pair (Path_Name Lit)
                                                     (Path_Pat Lit)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Lit for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Match
        where type PathType FieldPat Match = Path_Pair (Path_Name Match)
                                                       (Path_Pat Match)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Match for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat ModName
        where type PathType FieldPat
                            ModName = Path_Pair (Path_Name ModName) (Path_Pat ModName)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal ModName for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Name
        where type PathType FieldPat Name = Path_Pair (Path_Name Name)
                                                      (Path_Pat Name)
              toLens (Path_First _) = _1
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Name for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat NameFlavour
        where type PathType FieldPat
                            NameFlavour = Path_Pair (Path_Name NameFlavour)
                                                    (Path_Pat NameFlavour)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal NameFlavour for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat NameSpace
        where type PathType FieldPat
                            NameSpace = Path_Pair (Path_Name NameSpace) (Path_Pat NameSpace)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal NameSpace for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat OccName
        where type PathType FieldPat
                            OccName = Path_Pair (Path_Name OccName) (Path_Pat OccName)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal OccName for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Pat
        where type PathType FieldPat Pat = Path_Pair (Path_Name Pat)
                                                     (Path_Pat Pat)
              toLens (Path_Second _) = _2
              toLens u = error $ ("Unexpected goal Pat for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Phases
        where type PathType FieldPat Phases = Path_Pair (Path_Name Phases)
                                                        (Path_Pat Phases)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Phases for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat PkgName
        where type PathType FieldPat
                            PkgName = Path_Pair (Path_Name PkgName) (Path_Pat PkgName)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal PkgName for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Pragma
        where type PathType FieldPat Pragma = Path_Pair (Path_Name Pragma)
                                                        (Path_Pat Pragma)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Pragma for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Range
        where type PathType FieldPat Range = Path_Pair (Path_Name Range)
                                                       (Path_Pat Range)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Range for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Role
        where type PathType FieldPat Role = Path_Pair (Path_Name Role)
                                                      (Path_Pat Role)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Role for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat RuleBndr
        where type PathType FieldPat
                            RuleBndr = Path_Pair (Path_Name RuleBndr) (Path_Pat RuleBndr)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal RuleBndr for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat RuleMatch
        where type PathType FieldPat
                            RuleMatch = Path_Pair (Path_Name RuleMatch) (Path_Pat RuleMatch)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal RuleMatch for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Safety
        where type PathType FieldPat Safety = Path_Pair (Path_Name Safety)
                                                        (Path_Pat Safety)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Safety for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Stmt
        where type PathType FieldPat Stmt = Path_Pair (Path_Name Stmt)
                                                      (Path_Pat Stmt)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Stmt for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Strict
        where type PathType FieldPat Strict = Path_Pair (Path_Name Strict)
                                                        (Path_Pat Strict)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Strict for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat TyLit
        where type PathType FieldPat TyLit = Path_Pair (Path_Name TyLit)
                                                       (Path_Pat TyLit)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal TyLit for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat TySynEqn
        where type PathType FieldPat
                            TySynEqn = Path_Pair (Path_Name TySynEqn) (Path_Pat TySynEqn)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal TySynEqn for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat TyVarBndr
        where type PathType FieldPat
                            TyVarBndr = Path_Pair (Path_Name TyVarBndr) (Path_Pat TyVarBndr)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal TyVarBndr for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat String
        where type PathType FieldPat String = Path_Pair (Path_Name String)
                                                        (Path_Pat String)
              toLens (Path_First v) = _1 . toLens v
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Char] (aka String) for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Rational
        where type PathType FieldPat
                            Rational = Path_Pair (Path_Name Rational) (Path_Pat Rational)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Ratio Integer (aka Rational) for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Cxt
        where type PathType FieldPat Cxt = Path_Pair (Path_Name Cxt)
                                                     (Path_Pat Cxt)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Type] (aka Cxt) for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat FieldExp
        where type PathType FieldPat
                            FieldExp = Path_Pair (Path_Name FieldExp) (Path_Pat FieldExp)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal (Name, Exp) (aka FieldExp) for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat FieldPat
        where type PathType FieldPat
                            FieldPat = Path_Pair (Path_Name FieldPat) (Path_Pat FieldPat)
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal (Name, Pat) (aka FieldPat) for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat Type
        where type PathType FieldPat Type = Path_Pair (Path_Name Type)
                                                      (Path_Pat Type)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Type (aka Kind, aka Pred) for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat StrictType
        where type PathType FieldPat
                            StrictType = Path_Pair (Path_Name StrictType) (Path_Pat StrictType)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal (Strict, Type) (aka StrictType) for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path FieldPat VarStrictType
        where type PathType FieldPat
                            VarStrictType = Path_Pair (Path_Name VarStrictType)
                                                      (Path_Pat VarStrictType)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal (Name, Strict, Type) (aka VarStrictType) for (Name, Pat) (aka FieldPat): " ++ show u)
    instance Path Type Type
        where type PathType Type Type = Path_Type Type
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal Type (aka Kind, aka Pred) for Type (aka Kind, aka Pred): " ++ show u)
    instance Path StrictType ([TyVarBndr])
        where type PathType StrictType
                            ([TyVarBndr]) = Path_Pair (Path_Strict ([TyVarBndr]))
                                                      (Path_Type ([TyVarBndr]))
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [TyVarBndr] for (Strict, Type) (aka StrictType): " ++ show u)
    instance Path StrictType Int
        where type PathType StrictType Int = Path_Pair (Path_Strict Int)
                                                       (Path_Type Int)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Int for (Strict, Type) (aka StrictType): " ++ show u)
    instance Path StrictType BigNat
        where type PathType StrictType
                            BigNat = Path_Pair (Path_Strict BigNat) (Path_Type BigNat)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal BigNat for (Strict, Type) (aka StrictType): " ++ show u)
    instance Path StrictType Integer
        where type PathType StrictType
                            Integer = Path_Pair (Path_Strict Integer) (Path_Type Integer)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Integer for (Strict, Type) (aka StrictType): " ++ show u)
    instance Path StrictType ModName
        where type PathType StrictType
                            ModName = Path_Pair (Path_Strict ModName) (Path_Type ModName)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal ModName for (Strict, Type) (aka StrictType): " ++ show u)
    instance Path StrictType Name
        where type PathType StrictType Name = Path_Pair (Path_Strict Name)
                                                        (Path_Type Name)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal Name for (Strict, Type) (aka StrictType): " ++ show u)
    instance Path StrictType NameFlavour
        where type PathType StrictType
                            NameFlavour = Path_Pair (Path_Strict NameFlavour)
                                                    (Path_Type NameFlavour)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal NameFlavour for (Strict, Type) (aka StrictType): " ++ show u)
    instance Path StrictType NameSpace
        where type PathType StrictType
                            NameSpace = Path_Pair (Path_Strict NameSpace) (Path_Type NameSpace)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal NameSpace for (Strict, Type) (aka StrictType): " ++ show u)
    instance Path StrictType OccName
        where type PathType StrictType
                            OccName = Path_Pair (Path_Strict OccName) (Path_Type OccName)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal OccName for (Strict, Type) (aka StrictType): " ++ show u)
    instance Path StrictType PkgName
        where type PathType StrictType
                            PkgName = Path_Pair (Path_Strict PkgName) (Path_Type PkgName)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal PkgName for (Strict, Type) (aka StrictType): " ++ show u)
    instance Path StrictType Strict
        where type PathType StrictType
                            Strict = Path_Pair (Path_Strict Strict) (Path_Type Strict)
              toLens (Path_First _) = _1
              toLens u = error $ ("Unexpected goal Strict for (Strict, Type) (aka StrictType): " ++ show u)
    instance Path StrictType TyLit
        where type PathType StrictType
                            TyLit = Path_Pair (Path_Strict TyLit) (Path_Type TyLit)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal TyLit for (Strict, Type) (aka StrictType): " ++ show u)
    instance Path StrictType TyVarBndr
        where type PathType StrictType
                            TyVarBndr = Path_Pair (Path_Strict TyVarBndr) (Path_Type TyVarBndr)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal TyVarBndr for (Strict, Type) (aka StrictType): " ++ show u)
    instance Path StrictType String
        where type PathType StrictType
                            String = Path_Pair (Path_Strict String) (Path_Type String)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Char] (aka String) for (Strict, Type) (aka StrictType): " ++ show u)
    instance Path StrictType Cxt
        where type PathType StrictType Cxt = Path_Pair (Path_Strict Cxt)
                                                       (Path_Type Cxt)
              toLens (Path_Second v) = _2 . toLens v
              toLens u = error $ ("Unexpected goal [Type] (aka Cxt) for (Strict, Type) (aka StrictType): " ++ show u)
    instance Path StrictType Type
        where type PathType StrictType Type = Path_Pair (Path_Strict Type)
                                                        (Path_Type Type)
              toLens (Path_Second _) = _2
              toLens u = error $ ("Unexpected goal Type (aka Kind, aka Pred) for (Strict, Type) (aka StrictType): " ++ show u)
    instance Path StrictType StrictType
        where type PathType StrictType
                            StrictType = Path_Pair (Path_Strict StrictType)
                                                   (Path_Type StrictType)
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal (Strict, Type) (aka StrictType) for (Strict, Type) (aka StrictType): " ++ show u)
    instance Path VarStrictType VarStrictType
        where type PathType VarStrictType
                            VarStrictType = Path_VarStrictType VarStrictType
              toLens _ = iso id id
              toLens u = error $ ("Unexpected goal (Name, Strict, Type) (aka VarStrictType) for (Name, Strict, Type) (aka VarStrictType): " ++ show u)
  |]
