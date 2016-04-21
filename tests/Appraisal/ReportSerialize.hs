{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Appraisal.ReportSerialize where

import Appraisal.Report
import Appraisal.ReportItem
import Appraisal.ImageFile
import Data.Serialize
import Data.Text
import GHC.Generics
import Language.Haskell.TH.Path.Order

deriving instance Generic ReportFieldLabel
instance Serialize  ReportFieldLabel
deriving instance Generic Author
instance Serialize  Author
deriving instance Generic AuthorFieldLabel
instance Serialize  AuthorFieldLabel
deriving instance Generic ReportValueTypeInfo
instance Serialize  ReportValueTypeInfo
deriving instance Generic ReportIntendedUse
instance Serialize  ReportIntendedUse
deriving instance Generic ReportValueApproachInfo
instance Serialize  ReportValueApproachInfo
deriving instance Generic ReportElem
instance Serialize  ReportElem
deriving instance Generic ReportStatus
instance Serialize  ReportStatus
deriving instance Generic Branding
instance Serialize  Branding
deriving instance Generic ReportStandard
instance Serialize  ReportStandard
deriving instance Generic Report
instance Serialize  Report
deriving instance Generic ReportElemTypeName
instance Serialize  ReportElemTypeName

deriving instance Generic Item
instance Serialize  Item
deriving instance Generic ImageFile
instance Serialize  ImageFile
deriving instance Generic ImageType
instance Serialize  ImageType

-- deriving instance (Generic a, Generic b) => Generic (Order a b)
-- instance (Serialize a, Serialize b) => Serialize (Order a b)
deriving instance Generic AbbrevPairID
instance Serialize AbbrevPairID
