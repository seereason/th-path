{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift (Lift(lift), deriveLiftMany)
import Language.Haskell.TH.Path.Core (Path_Pair, Path_Map, Path_Maybe, Path_Either)
import Language.Haskell.TH.Path.Decs (allDecsToFile)
import Language.Haskell.TH.Path.Order (Path_OMap)
import Language.Haskell.TH.Syntax (runIO, runQ)
import System.FilePath.Find ((==?), (&&?), always, extension, fileType, FileType(RegularFile), find)
import Text.LaTeX hiding (lift)
import Text.LaTeX.Base.Syntax
import Text.Pandoc (Pandoc, Meta, MetaValue, QuoteType, Inline, Format, MathType, Citation,
                    CitationMode, Block, ListNumberStyle, ListNumberDelim, Alignment)

$(runQ (runIO (find always (extension ==? ".hs" &&? fileType ==? RegularFile) "Language/Haskell/TH/Path")) >>=
  allDecsToFile sort [ [t|ReportMap|] ] (Just "tests/ReportHead.hs") Nothing "tests/ReportDecs.hs")

instance Lift Text where
    lift = lift . unpack

-- | Most of these should be moved back to live with their parent types.
$(deriveLiftMany [''UserId, ''JSONText, ''Permissions, ''CIString, ''ImageFile, ''ImageCrop, ''ImageType, ''Markup, ''Pandoc, ''Meta, ''MetaValue, ''Inline, ''QuoteType, ''Text.Pandoc.MathType, ''Format, ''Citation, ''CitationMode, ''Block, ''ListNumberStyle, ''ListNumberDelim, ''Alignment])
$(deriveLiftMany [''LaTeX, ''Units, ''Dimension, ''ImageSize, ''ReportImageID, ''ReportImage, ''Item, ''UUID, ''ReportValueTypeInfo, ''ReportValueApproachInfo, ''ReportStatus, ''ReportStandard, ''ReportIntendedUse, ''ReportFlags, ''ReportElemID, ''ReportElem, ''Report, ''MarkupPairID, ''MarkupID, ''Branding, ''AuthorID, ''Author, ''AbbrevPairID, ''TeXArg, ''SaneSize, ''ReportView, ''Measure, ''ReportImageView, ''ReadOnly, ''Text.LaTeX.Base.Syntax.MathType, ''UPath_Report, ''ItemFieldName, ''UPath_ReportView, ''Path_OMap, ''Path_Pair, ''UPath_UUID, ''UPath_Markup, ''UPath_Text, ''UPath_ReportValueApproachInfo, ''UPath_JSONText, ''UPath_ReportStatus, ''UPath_String, ''UPath_ReportStandard, ''UPath_Int, ''UPath_ReportFlags, ''UPath_Bool, ''UPath_ReportElem, ''UPath_Item, ''UPath_ReadOnlyFilePath, ''Path_Map, ''UPath_Permissions, ''UPath_UserIds, ''UPath_MaybeReportIntendedUse, ''UPath_ReportImage, ''UPath_ReportValueTypeInfo, ''UPath_ReportImageView, ''UPath_UserId, ''UPath_Integer, ''Path_Maybe, ''UPath_Int64, ''Path_Either, ''UPath_CIString, ''UPath_URI, ''UPath_Branding, ''UPath_SaneSizeImageSize, ''UPath_Author, ''UPath_ImageSize, ''UPath_MaybeImageFile, ''UPath_Units, ''UPath_ImageFile, ''UPath_Double, ''UPath_ImageCrop, ''UPath_Dimension])
-- instance Lift a => Lift (Path_ReportElems a) where lift = undefined
-- instance Lift a => Lift (Path_MarkupPairs a) where lift = undefined
#endif
