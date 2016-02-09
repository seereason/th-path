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
import Appraisal.File (File, FileSource)
import Appraisal.Markup (Markup)
import Appraisal.Permissions (Permissions)
import Appraisal.ReportTH (decs)
import Appraisal.ReportImage (ReportImageID, ReportImage)
import Appraisal.Report (ReportStatus, ReportValueTypeInfo, ReportValueApproachInfo, ReportStandard, ReportIntendedUse,
                         ReportFlags, ReportElemID, ReportElem, Report, MarkupPairID, MarkupID, Branding, AuthorID, Author, AbbrevPairID)
import Appraisal.ReportItem (Item, ItemFieldName)
import Appraisal.ReportInstances (SaneSize, ReportView, ReportImageView, ReadOnly)
import Appraisal.Utils.CIString (CIString)
import Data.UserId (UserId)
import Data.UUID (UUID)
import Data.Text (unpack)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift (Lift(lift), deriveLiftMany)
import Language.Haskell.TH.Path.Core (Path_Pair, Path_Map, Path_Maybe, Path_Either)
import Language.Haskell.TH.Path.Order (Order, Path_OMap)
import Network.URI (URIAuth, URI)
import Text.LaTeX hiding (lift)
import Text.LaTeX.Base.Syntax
import Text.Pandoc (Pandoc, Meta, MetaValue, QuoteType, Inline, Format, MathType, Citation,
                    CitationMode, Block, ListNumberStyle, ListNumberDelim, Alignment)

$(pure decs)

instance Lift Text where
    lift = lift . unpack

-- | Most of these should be moved back to live with their parent types.
$(deriveLiftMany [''URIAuth, ''UserId, ''URI, ''Order, ''JSONText, ''Permissions, ''CIString, ''ImageFile, ''File, ''FileSource, ''ImageCrop, ''ImageType, ''Markup, ''Pandoc, ''Meta, ''MetaValue, ''Inline, ''QuoteType, ''Text.Pandoc.MathType, ''Format, ''Citation, ''CitationMode, ''Block, ''ListNumberStyle, ''ListNumberDelim, ''Alignment])
$(deriveLiftMany [''LaTeX, ''Units, ''Dimension, ''ImageSize, ''ReportImageID, ''ReportImage, ''Item, ''UUID, ''ReportValueTypeInfo, ''ReportValueApproachInfo, ''ReportStatus, ''ReportStandard, ''ReportIntendedUse, ''ReportFlags, ''ReportElemID, ''ReportElem, ''Report, ''MarkupPairID, ''MarkupID, ''Branding, ''AuthorID, ''Author, ''AbbrevPairID, ''TeXArg, ''SaneSize, ''ReportView, ''Measure, ''ReportImageView, ''ReadOnly, ''Text.LaTeX.Base.Syntax.MathType, ''Path_Report, ''ItemFieldName, ''Path_ReportView, ''Path_OMap, ''Path_Pair, ''Path_UUID, ''Path_Markup, ''Path_Text, ''Path_ReportValueApproachInfo, ''Path_JSONText, ''Path_ReportStatus, ''Path_String, ''Path_ReportStandard, ''Path_Int, ''Path_ReportFlags, ''Path_Bool, ''Path_ReportElem, ''Path_Item, ''Path_ReadOnlyFilePath, ''Path_Map, ''Path_Permissions, ''Path_UserIds, ''Path_MaybeReportIntendedUse, ''Path_ReportImage, ''Path_ReportValueTypeInfo, ''Path_ReportImageView, ''Path_UserId, ''Path_Integer, ''Path_Maybe, ''Path_Int64, ''Path_Either, ''Path_CIString, ''Path_URI, ''Path_Branding, ''Path_SaneSizeImageSize, ''Path_Author, ''Path_ImageSize, ''Path_MaybeImageFile, ''Path_Units, ''Path_ImageFile, ''Path_Double, ''Path_ImageCrop, ''Path_Dimension])
-- instance Lift a => Lift (Path_ReportElems a) where lift = undefined
-- instance Lift a => Lift (Path_MarkupPairs a) where lift = undefined
#endif
