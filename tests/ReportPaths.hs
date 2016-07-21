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
import Appraisal.ReportInstances (SaneSize, ReportView, ReportImageView, ReadOnly)
import Appraisal.ReportMap (ReportMap)
import Data.List (sort)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift (deriveLiftMany)
import Language.Haskell.TH.Path.Decs (derivePaths, writePaths)
import Language.Haskell.TH.Syntax (runIO, runQ)
import System.FilePath.Find ((==?), (&&?), always, extension, fileType, FileType(RegularFile), find)
import Text.LaTeX hiding (lift)

$(do deps <- runQ (runIO (find always (extension ==? ".hs" &&? fileType ==? RegularFile) "Language/Haskell/TH/Path"))
     decs <- sort <$> derivePaths [ [t|ReportMap|] ]
     runQ (runIO (writeFile "tests/ReportHs.hs" (unlines (map show decs))))
     hd <- runIO $ readFile "tests/ReportHead.hs"
     writePaths hd mempty "tests/ReportDecs.hs" deps decs)

-- | Most (all?) of these should be moved back to live with their parent types.
$(deriveLiftMany [''JSONText])
$(deriveLiftMany [''SaneSize, ''ReportView, ''ReportImageView, ''ReadOnly])
#endif
