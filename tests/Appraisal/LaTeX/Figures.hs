
{-# LANGUAGE CPP, DeriveDataTypeable, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Appraisal.LaTeX.Figures
    ( figures
    , PicName(..)
    , picture
    , Figure
    ) where

import qualified Appraisal.Image as I (ImageSize, latexSize, latexWidth, widthInInches)
import Appraisal.LaTeX
import Appraisal.LaTeX.Float (figure, FloatPlacement(HereDefinitely), subfigure, caption)
import Appraisal.ImageFile (extension, ImageFile(imageFileType))
import qualified Appraisal.ImageFile as I (ImageFile)
import Data.Generics (Data, Typeable)
import qualified Data.List as L (intersperse)
import Data.Monoid ((<>))
import qualified Data.Text as T (unpack, Text)
import Text.LaTeX.Base.Commands (raw)
import Text.LaTeX.Base.Syntax (protectText)
import Text.LaTeX.Base.Writer (LaTeXT)
import Text.LaTeX.Packages.Graphicx (includegraphics)

-- data Figure
--     = Figure
--       { imageFile :: I.ImageFile
--       , imageSize :: I.ImageSize
--       , imageName :: PicName
--       , imageCaption :: Maybe T.Text
--       }

type Figure = (I.ImageFile, I.ImageSize, PicName, Maybe T.Text)

-- The string is one of the checksums in /srv/reportserver/8000/datafiles
newtype PicName = PicName T.Text deriving (Read, Show, Eq, Ord, Typeable, Data)

figures :: forall m. Monad m => Double -> [Figure] -> LaTeXT m ()
figures textWidthInInches triples =
        renderRows (rows 0.0 [] triples)
        where
          -- Group figures into lists of total width <= textWidthInInches.  Watch
          -- out for case where a single image width is > textWidthInInches.
          rows :: Double -> [Figure] -> [Figure] -> [[Figure]]
          rows _ thisrow [] = [reverse thisrow]
          rows width thisrow more@(triple@(p, sz, PicName _name, _caption) : more') =
              let width' = width + sp + I.widthInInches p sz in
              if thisrow == [] || width' <= textWidthInInches
              then rows width' (triple : thisrow) more'
              else (reverse thisrow) : rows 0.0 [] more
#if 1
          renderRows :: [[Figure]] -> LaTeXT m ()
          renderRows = sequence_ . map renderRow
          renderRow :: [Figure] -> LaTeXT m ()
          renderRow row =
              figure (Just HereDefinitely)
                     (centering <> nocomment <>
                      sequence_ (L.intersperse (nbsp <> nocomment) (map renderFigure row))) <>
                     nocomment
          renderFigure :: Figure -> LaTeXT m ()
          renderFigure (p, sz, PicName name, cap) =
              subfigure (I.latexWidth p sz) $ do
                 nocomment
                 centering
                 includegraphics [I.latexSize p sz] (T.unpack name <> extension (imageFileType p))
                 nocomment
                 maybe mempty (\ cap' -> caption Nothing (raw . protectText $ cap') <> nocomment) cap
#else
          renderRows :: [[Figure]] -> LaTeXT m ()
          renderRows rows = mconcat (map renderRow rows)
          renderRow :: [Figure] -> LaTeXT m ()
          renderRow row =
              figure (Just HereDefinitely) (mconcat (map renderFigure row))
          renderFigure :: Figure -> LaTeXT m ()
          renderFigure (p, sz, PicName name, cap) =
              subfigure (width p sz) (size p sz) (name <> T.pack (extension (imageFileType p))) <>
              maybe mempty caption cap
          width p sz = raw "unimplemented: width"
          size p sz = raw "unimplemented: size"
          subfigure :: LaTeXC l => l -> l -> T.Text -> l
          subfigure _ _ _ = raw "unimplemented: subfigure"
          caption :: LaTeXC l => T.Text -> l
          caption _ = raw "unimplemented: caption"
#endif
          sp = 0.05

picture :: Monad m => I.ImageFile -> I.ImageSize -> PicName -> LaTeXT m ()
picture p sz (PicName name) =
    includegraphics [I.latexSize p sz] (T.unpack name <> extension (imageFileType p))
