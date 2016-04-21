{-# LANGUAGE OverloadedStrings #-}
module Tests.Report (report, image) where

import Appraisal.Markup (rawMarkdown, rawHtml)
import Appraisal.Permissions (Permissions(Permissions, owner, writers, readers))
import Appraisal.Report -- (Report(..), AbbrevPairID(..), AuthorID(..), Author(..), ReportElemID(..), ReportElem(..))
import Appraisal.ReportImage
import Appraisal.ReportItem
import Appraisal.Utils.CIString
import Data.Default (def)
import Language.Haskell.TH.Path.Order (fromPairs)

import Data.UserId
import Data.Maybe (fromJust)
import Data.Map (fromList)

import Appraisal.File
import Appraisal.Image
import Appraisal.ImageFile

import Data.UUID as UUID (fromString)

report :: Report
report = def

image :: ReportImage
image = def
