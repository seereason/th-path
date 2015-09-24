{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables, TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-unused-binds #-}
module Appraisal.Abbrevs
    ( foldTagsP
    , foldTagsM
    , expandTextRecursiveP
    , expandMarkupP
    , AbbrevsT
    , runAbbrevsT
    , runAbbrevsT'
    , AbbrevsM
    , runAbbrevsM
    , runAbbrevsM'
    , MonadAbbrevs
    , withAbbrevs
    , expandText
    , expandMarkup
    , tests
    ) where

import Appraisal.Markup as M (Markup, foldMarkup, rawMarkdown, rawHtml, rawLaTeX, rawPandoc)
import Appraisal.Utils.CIString (CIString)
import Appraisal.Utils.IsText (fromText)
import Control.Monad.RWS
import Data.Map as Map (Map, fromList, lookup)
import Data.String (fromString)
import qualified Data.Text as T
import Test.HUnit (Test(TestList, TestCase), assertEqual)

type AbbrevsT m = RWST (Map CIString Markup) () [CIString] m
type AbbrevsM = RWS (Map CIString Markup) () [CIString]

class (MonadReader (Map CIString Markup) m, MonadState [CIString] m) => MonadAbbrevs m

instance Monad m => MonadAbbrevs (AbbrevsT m)

withAbbrevs :: MonadAbbrevs m => (Map CIString Markup -> [CIString] -> a) -> m a
withAbbrevs action = ask >>= \mp -> get >>= \stk -> return $ action mp stk

runAbbrevsT :: Monad m => Map CIString Markup -> AbbrevsT m a -> m a
runAbbrevsT abbrevs action = evalRWST action abbrevs [] >>= return . fst

runAbbrevsT' :: Monad m => Map CIString Markup -> [CIString] -> AbbrevsT m a -> m a
runAbbrevsT' abbrevs stk action = evalRWST action abbrevs stk >>= return . fst

runAbbrevsM :: Map CIString Markup -> AbbrevsM a -> a
runAbbrevsM abbrevs action = fst $ evalRWS action abbrevs []

runAbbrevsM' :: Map CIString Markup -> [CIString] -> AbbrevsM a -> a
runAbbrevsM' abbrevs stk action = fst $ evalRWS action abbrevs stk

data Expansion
    = Undefined
    | Loop
    | Expansion Markup
    deriving (Show)

push :: MonadState [CIString] m => CIString -> m a -> m a
-- push tag action = local (\ x -> x {stack = tag : (stack x)}) action
push tag action =
    do stk <- get
       let stk' = tag : stk
       put stk'
       r <- action
       put stk
       return r

pushP :: CIString -> [CIString] -> ([CIString] -> a) -> a
pushP tag stk f = f (tag : stk)

expandTagP :: (Map CIString Markup) -> [CIString] -> CIString -> Expansion
expandTagP mp stk tag =
    if elem tag stk
    then Loop
    else case Map.lookup tag mp of
           Just markup -> Expansion markup
           Nothing -> Undefined

expandTag :: (MonadReader (Map CIString Markup) m, MonadState [CIString] m) => CIString -> m Expansion
expandTag tag =
    do mp <- ask
       stk <- get
       return $ if elem tag stk
                then Loop
                else case Map.lookup tag mp of
                       Just markup -> Expansion markup
                       Nothing -> Undefined

foldTagsP :: Monoid a =>
             (T.Text -> a)                   -- Handle a piece of text with no tags
          -> (T.Text -> CIString -> a)       -- Perform the expansion of a tag
          -> T.Text                          -- The input text
          -> a
foldTagsP doText doTag t =
    case T.break (== '[') t of
      (_, more) | T.null more -> doText t -- No more tag start markers
      (pre, more) ->
          case T.break (== ']') (T.tail more) of
            (_, post) | T.null post -> doText t -- No more tag end markers
            (tag, post) ->
                let tag' = fromText tag :: CIString in
                doText pre <>
                doTag tag tag' <>
                foldTagsP doText doTag (T.tail post)

foldTagsM :: (Monad m, Monoid a) =>
             (T.Text -> m a)                   -- Handle a piece of text with no tags
          -> (T.Text -> CIString -> m a)       -- Perform the expansion of a tag
          -> T.Text                          -- The input text
          -> m a
foldTagsM doText doTag t =
    case T.break (== '[') t of
      (_, more) | T.null more -> doText t -- No more tag start markers
      (pre, more) ->
          case T.break (== ']') (T.tail more) of
            (_, post) | T.null post -> doText t -- No more tag end markers
            (tag, post) -> do
              let tag' = fromText tag :: CIString
              a1 <- doText pre
              a2 <- doTag tag tag'
              a3 <- foldTagsM doText doTag (T.tail post)
              return $ a1 <> a2 <> a3

-- | Recursively expand the tags in a block of text.
expandTextRecursiveP
    :: Monoid a =>
       Map CIString Markup
    -> [CIString]
    -> (T.Text -> a)                -- Handle a piece of text with no tags
    -> (T.Text -> a)                -- Handle an undefined tag
    -> (T.Text -> a)                -- Handle a loop in the tag definitions
    -> (Markup -> Map CIString Markup -> [CIString] -> a) -- Perform the expansion of a tag
    -> T.Text                       -- The input text
    -> a
expandTextRecursiveP mp stk doText doUndef doLoop doTag t =
    foldTagsP
      doText
      (\ tag tag' ->
           case expandTagP mp stk tag' of
             Loop -> doLoop tag
             Undefined -> doUndef tag
             Expansion markup -> pushP tag' stk (doTag markup mp))
      t

-- | Recursively expand the tags that appear in a piece of text.
expandText :: (MonadReader (Map CIString Markup) m, Monoid a, MonadState [CIString] m) =>
              (T.Text -> m a) -- Handle a piece of text with no tags
           -> (T.Text -> m a) -- Handle an undefined tag
           -> (T.Text -> m a) -- Handle a loop in the tag definitions
           -> (Markup -> m a) -- Perform the expansion of a tag
           -> T.Text          -- The input text
           -> m a
expandText doText doUndef doLoop doTag t =
    foldTagsM
      doText
      (\ tag tag' ->
           expandTag tag' >>= \ result ->
           case result of
             Loop -> doLoop tag
             Undefined -> doUndef tag
             Expansion markup -> push tag' (doTag markup))
      t

-- | Recursively expand the tags that appear in a piece of text.
expandText' :: (MonadReader (Map CIString Markup) m, Monoid a, MonadState [CIString] m) =>
               (T.Text -> m a) -- Handle a piece of text with no tags
            -> (T.Text -> m a) -- Handle an undefined tag
            -> (T.Text -> m a) -- Handle a loop in the tag definitions
            -> (Markup -> m a) -- Process the expanded markup
            -> T.Text          -- The input text
            -> m a
expandText' doText doUndef doLoop doTag t =
    foldTagsM
      doText
      (\ tag tag' ->
           expandTag tag' >>= \ result ->
           case result of
             Loop -> doLoop tag
             Undefined -> doUndef tag
             Expansion markup -> push tag' (doTag markup))
      t

expandMarkupP
    :: Markup
    -> Map CIString Markup
    -> [CIString]
    -> Markup
expandMarkupP markup mp stk =
    foldMarkup
      (expandTextRecursiveP
         mp
         stk
         rawMarkdown
         (\tag -> rawMarkdown $ "[" <> tag <> "]")
         (\tag -> rawMarkdown $ "[abbreviation loop: " <> tag <> "]")
         expandMarkupP)
      (expandTextRecursiveP
         mp
         stk
         rawHtml
         (\tag -> rawHtml $ "[" <> tag <> "]")
         (\tag -> rawHtml $ "[abbreviation loop: " <> tag <> "]")
         expandMarkupP)
      rawLaTeX
      rawPandoc
      (mconcat . map (\ markup' -> expandMarkupP markup' mp stk))
      markup

expandMarkup :: (MonadReader (Map CIString Markup) m, MonadState [CIString] m) =>
                (Markup -> m Markup)
             -> Markup -> m Markup
expandMarkup doTag markup =
    foldMarkup
      (expandText'
        (return . rawMarkdown)
        (\tag -> return $ rawMarkdown $ "[" <> tag <> "]")
        (\tag -> return $ rawMarkdown $ "[abbreviation loop: " <> tag <> "]")
        (expandMarkup doTag))
      (expandText'
        (return  . rawHtml)
        (\tag -> return $ rawHtml $ "[" <> tag <> "]")
        (\tag -> return $ rawHtml $ "[abbreviation loop: " <> tag <> "]")
        (expandMarkup doTag))
      (return . rawLaTeX)
      (return . rawPandoc)
      (\ ms -> mapM (expandMarkup doTag) ms >>= return . mconcat)
      markup

tests :: Test
tests =
    TestList
    [ TestCase $
        assertEqual
          "plain text"
          (rawMarkdown "plain text")
          (runAbbrevsM
            (fromList [])
            (expandMarkup return (rawMarkdown "plain text")))
    , TestCase $
        assertEqual
          "undefined tag"
          (rawMarkdown "expand Expansion of X here [Y]")
          (runAbbrevsM
            (fromList [(fromString "X", rawMarkdown "Expansion of X")])
            (expandMarkup return (rawMarkdown "expand [X] here [Y]")))
    , TestCase $
        assertEqual
          "one node loop"
          (rawMarkdown "expand Expansion of X contains itself [abbreviation loop: X] here")
          (runAbbrevsM
            (fromList [(fromString "X", rawMarkdown "Expansion of X contains itself [X]")])
            (expandMarkup return (rawMarkdown "expand [X] here")))
    , TestCase $
        assertEqual
          "two step expansion"
          (rawMarkdown "expand Expansion of X contains Expansion of Y here")
          (runAbbrevsM
            (fromList [(fromString "X", rawMarkdown "Expansion of X contains [Y]"),
                       (fromString "Y", rawMarkdown "Expansion of Y")])
            (expandMarkup return (rawMarkdown "expand [X] here")))
    , TestCase $
        assertEqual
          "two node loop"
          (rawMarkdown "expand Expansion of X contains Expansion of Y contains [abbreviation loop: X] here")
          (runAbbrevsM
            (fromList [(fromString "X", rawMarkdown "Expansion of X contains [Y]"),
                       (fromString "Y", rawMarkdown "Expansion of Y contains [X]")])
            (expandMarkup return (rawMarkdown "expand [X] here")))
    ]
