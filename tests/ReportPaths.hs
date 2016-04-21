{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
#if 0
module ReportPaths (module ReportDecs) where
import ReportDecs

#else
module ReportPaths where

import Appraisal.IntJS (JSONText)
import Appraisal.ImageFile (ImageFile, ImageType)
import Appraisal.Image (ImageCrop, Units, Dimension, ImageSize)
import Appraisal.Markup (Markup)
import Appraisal.Permissions (Permissions)
import Appraisal.ReportImage (ReportImageID, ReportImage)
import Appraisal.Report (ReportStatus, ReportValueTypeInfo, ReportValueApproachInfo, ReportStandard, ReportIntendedUse,
                         ReportFlags, ReportElemID, ReportElem, Report, MarkupPairID, MarkupID, Branding, AuthorID, Author, AbbrevPairID)
import Appraisal.ReportItem (Item, ItemFieldName)
import Appraisal.ReportInstances (SaneSize, ReportView, ReportImageView, ReadOnly)
import Appraisal.ReportMap (ReportMap)
import Appraisal.Utils.CIString (CIString)
import Data.List (sort)
import Data.UserId (UserId)
import Data.UUID (UUID)
import Data.Text (unpack)
import GHC.Generics (Generic)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift (Lift(lift), deriveLiftMany)
import Language.Haskell.TH.Path.Decs (derivePaths, writePaths)
import Language.Haskell.TH.Syntax (runIO, runQ)
import System.FilePath.Find ((==?), (&&?), always, extension, fileType, FileType(RegularFile), find)
import Text.LaTeX hiding (lift)
import Text.LaTeX.Base.Syntax
import Text.Pandoc (Pandoc, Meta, MetaValue, QuoteType, Inline, Format, MathType, Citation,
                    CitationMode, Block, ListNumberStyle, ListNumberDelim, Alignment)

$(do deps <- runQ (runIO (find always (extension ==? ".hs" &&? fileType ==? RegularFile) "Language/Haskell/TH/Path"))
     decs <- sort <$> derivePaths [ [t|ReportMap|] ]
     runQ (runIO (writeFile "tests/ReportHs.hs" (unlines (map show decs))))
     writePaths (Just "tests/ReportHead.hs") Nothing "tests/ReportDecs.hs" deps decs)

-- | Most (all?) of these should be moved back to live with their parent types.
$(deriveLiftMany [''JSONText])
$(deriveLiftMany [''SaneSize, ''ReportView, ''ReportImageView, ''ReadOnly])
#endif
