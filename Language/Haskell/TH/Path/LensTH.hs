-- | Modified copy of makeClassFor function from Control.Lens.TH in the lens package
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}
module Language.Haskell.TH.Path.LensTH
    ( makeClassyFor
    ) where

import Control.Lens ((&), lensClass, lensField, (.~), over, mapped, both)
import Control.Lens.Internal.FieldTH (DefName(TopName), LensRules(..))
import Data.Char (toLower)
import Language.Haskell.TH
import Language.Haskell.TH.Path.FieldTH (makeFieldOptics)

-- | Derive lenses and traversals, using a named wrapper class, and
-- specifying explicit pairings of @(fieldName, traversalName)@.
--
-- Example usage:
--
-- @
-- 'makeClassyFor' \"HasFoo\" \"foo\" [(\"_foo\", \"fooLens\"), (\"bar\", \"lbar\")] ''Foo
-- @
makeClassyFor :: String -> String -> [(String, String)] -> Name -> DecsQ
makeClassyFor clsName funName fields = makeFieldOptics $
  classyRulesFor (const (Just (clsName, funName))) fields

mkNameLookup :: [(String,String)] -> Name -> [Name] -> Name -> [DefName]
mkNameLookup kvs _ _ field =
  [ TopName (mkName v) | (k,v) <- kvs, k == nameBase field]

-- | Rules for making lenses and traversals that precompose another 'Lens'
-- using a custom function for naming the class, main class method, and a
-- mapping from field names to definition names.
classyRulesFor
  :: (String -> Maybe (String, String)) {- ^ Type Name -> Maybe (Class Name, Method Name) -} ->
  [(String, String)] {- ^ [(Field Name, Method Name)] -} ->
  LensRules
classyRulesFor classFun fields = classyRules
  & lensClass .~ (over (mapped . both) mkName . classFun . nameBase)
  & lensField .~ mkNameLookup fields

-- | Rules for making lenses and traversals that precompose another 'Lens'.
classyRules :: LensRules
classyRules = LensRules
  { _simpleLenses    = True
  , _generateSigs    = True
  , _generateClasses = True
  , _allowIsos       = False -- generating Isos would hinder "subtyping"
  , _allowUpdates    = True
  , _lazyPatterns    = False
  , _classyLenses    = \n ->
        case nameBase n of
          x:xs -> Just (mkName ("Has" ++ x:xs), mkName (toLower x:xs))
          []   -> Nothing
  , _fieldToDef      = \_ _ n ->
        case nameBase n of
          '_':x:xs -> [TopName (mkName (toLower x:xs))]
          _        -> []
  }
