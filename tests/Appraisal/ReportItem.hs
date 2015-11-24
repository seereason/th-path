{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Appraisal.ReportItem
    ( Item(..)
    , ItemFieldInfo(..)
    , itemFieldMap
    , ItemFieldName(..)
    , nullItem
    -- , changeItem
    -- , changeItem'
    ) where

import Appraisal.Currency(Priceable(..), parseCashValue)
import Appraisal.Markup (Markup, markupText)
import Appraisal.ReportImage(ReportImages)
import Data.Generics (Data, Typeable)
import Data.Map as Map (Map, fromList, lookup)
import Data.SafeCopy (deriveSafeCopy, base)
import qualified Data.Text as T
import Language.Haskell.TH.Path.Graph (SelfPath)
import Language.Haskell.TH.Path.Order as Order (empty)

data ItemFieldName
    = ItemLocation
    | ItemDataSheetNumber
    | ItemTypeOfObject
    | ItemArtistOrMaker
    | ItemArtistDate
    | ItemCountryOrNationality
    | ItemSubjectOrTitle
    | ItemSignatureInscriptionsMarkings
    | ItemMediumMaterialsTechniques
    | ItemDateOrPeriod
    | ItemSupportBaseFrame
    | ItemFrameMeasurement
    | ItemItemMeasurement
    | ItemProvenance
    | ItemCost
    | ItemCondition
    | ItemEdition
    | ItemDescription
    | ItemMarketAnalysis
    | ItemExhibitionsAndPublications
    | ItemAdditionalNotes
    | ItemCashValue
    deriving (Show, Read, Eq, Ord, Data, Typeable)

$(deriveSafeCopy 2 'base ''ItemFieldName)

instance SelfPath ItemFieldName

data Item
    = Item { itemName :: T.Text
           , fields :: Map ItemFieldName Markup
           , images :: ReportImages -- Use a typedef here so that paths like Path_Report ReportImages are generated
           } deriving (Show, Read, Eq, Ord, Data, Typeable)

--fieldLabel :: ItemFieldName -> T.Text
--fieldLabel name = T.pack $ maybe "Unknown" itemFieldLabel (lookup name itemFieldMap)

noFieldValue :: Markup
noFieldValue = mempty

data ItemFieldInfo = ItemFieldInfo { itemFieldName :: ItemFieldName
                                   , itemFieldLabel :: String
                                   , itemFieldHide :: Bool }

-- This will hold information about how to generate the HTML for a field, etc.
-- The order the elements appear in the list determines the order they will
-- appear in the input forms and in the report.
itemFieldMap :: Map ItemFieldName ItemFieldInfo
itemFieldMap =
    Map.fromList $ map (\ field -> (itemFieldName field, field)) $
    [ ItemFieldInfo {itemFieldName = ItemDataSheetNumber,               itemFieldLabel = "Item Name",                           itemFieldHide = False}
    , ItemFieldInfo {itemFieldName = ItemLocation,                      itemFieldLabel = "Location",                            itemFieldHide = False}
    , ItemFieldInfo {itemFieldName = ItemTypeOfObject,                  itemFieldLabel = "Type of Object",                      itemFieldHide = False}
    , ItemFieldInfo {itemFieldName = ItemMediumMaterialsTechniques,     itemFieldLabel = "Medium",                              itemFieldHide = False}
    , ItemFieldInfo {itemFieldName = ItemDateOrPeriod,                  itemFieldLabel = "Date/Period",                         itemFieldHide = False}
    , ItemFieldInfo {itemFieldName = ItemCountryOrNationality,          itemFieldLabel = "Country/Nationality",                 itemFieldHide = False}
    , ItemFieldInfo {itemFieldName = ItemArtistOrMaker,                 itemFieldLabel = "Artist/Maker",                        itemFieldHide = False}
    , ItemFieldInfo {itemFieldName = ItemArtistDate,                    itemFieldLabel = "Artist Life Date",                    itemFieldHide = False}
    , ItemFieldInfo {itemFieldName = ItemSubjectOrTitle,                itemFieldLabel = "Subject/Title",                       itemFieldHide = False}
    , ItemFieldInfo {itemFieldName = ItemSupportBaseFrame,              itemFieldLabel = "Support/Frame/Base",                  itemFieldHide = False}
    , ItemFieldInfo {itemFieldName = ItemFrameMeasurement,              itemFieldLabel = "Frame/Support Measurements",          itemFieldHide = False}
    , ItemFieldInfo {itemFieldName = ItemItemMeasurement,               itemFieldLabel = "Item Measurements",                   itemFieldHide = False}
    , ItemFieldInfo {itemFieldName = ItemSignatureInscriptionsMarkings, itemFieldLabel = "Signature/Inscription/Markings",      itemFieldHide = False}
    , ItemFieldInfo {itemFieldName = ItemCondition,                     itemFieldLabel = "Condition",                           itemFieldHide = False}
    , ItemFieldInfo {itemFieldName = ItemEdition,                       itemFieldLabel = "Edition",                             itemFieldHide = False}
    , ItemFieldInfo {itemFieldName = ItemProvenance,                    itemFieldLabel = "Provenance",                          itemFieldHide = False}
    , ItemFieldInfo {itemFieldName = ItemCost,                          itemFieldLabel = "Cost",                                itemFieldHide = False}
    , ItemFieldInfo {itemFieldName = ItemExhibitionsAndPublications,    itemFieldLabel = "Exhibitions/Publications",            itemFieldHide = False}
    , ItemFieldInfo {itemFieldName = ItemDescription,                   itemFieldLabel = "Description:",                        itemFieldHide = False}
    , ItemFieldInfo {itemFieldName = ItemMarketAnalysis,                itemFieldLabel = "Market Analysis:",                    itemFieldHide = False}
    , ItemFieldInfo {itemFieldName = ItemCashValue,                     itemFieldLabel = "Value",                               itemFieldHide = False}
    , ItemFieldInfo {itemFieldName = ItemAdditionalNotes,               itemFieldLabel = "Additional Notes",                    itemFieldHide = True}
    ]



nullItem :: Item
nullItem
    = Item { itemName = mempty
             , fields = mempty
             , images = empty }

{-
changeItem :: ItemFieldName -> FormText -> Item -> Item
changeItem field value item = setItemFieldValue field value item

setItemFieldValue :: ItemFieldName -> FormText -> Item -> Item
setItemFieldValue name value item =
    item {fields = mapInsert name value (fields item)}
    where
      mapInsert key value map = (key, T.pack (decodeFormText value)) : filter (\ (x, _) -> x /= key) map

changeItem' :: ItemFieldName -> T.Text -> Item -> Item
changeItem' field value item = setItemFieldValue' field value item

setItemFieldValue' :: ItemFieldName -> T.Text -> Item -> Item
setItemFieldValue' name value item =
    item {fields = mapInsert name value (fields item)}
    where
      mapInsert key value map = (key, value) : filter (\ (x, _) -> x /= key) map
-}

getItemFieldValue :: ItemFieldName -> Item -> Markup
getItemFieldValue name item =
    mapFindWithDefault noFieldValue (fields item)
    where
      mapFindWithDefault def mp =
          maybe def id (Map.lookup name mp)

{-
modifyItemFieldValue :: (T.Text -> T.Text) -> ItemFieldName -> Item -> Item
modifyItemFieldValue  f name item = setItemFieldValue' name (f (getItemFieldValue name item)) item
-}

instance Priceable Item where
    cashValue item = parseCashValue $ T.unpack $ markupText $ getItemFieldValue ItemCashValue item

$(deriveSafeCopy 7 'base ''Item)

