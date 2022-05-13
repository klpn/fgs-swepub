{-# LANGUAGE OverloadedLabels, OverloadedStrings, QuasiQuotes, DuplicateRecordFields, DeriveGeneric, RecordWildCards #-}

module ModsXML where

import           Cerif
import           Conduit
import           Control.Lens hiding (matching)
import           Data.Generics.Labels
import           Data.List
import           Text.Hamlet.XML
import           Text.XML
import           Text.XML.Stream.Parse
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.XML.Types as X

modsns :: T.Text
modsns = "http://www.loc.gov/mods/v3"

data ModsRecord = ModsRecord {
        recordInfo :: ModsRecordInfo
        , originInfo :: [ModsOriginInfo]
        , language :: [ModsLanguage]
        , titleInfo :: [ModsTitleInfo]
        , name :: [ModsName]
        , abstract :: [T.Text]
        , identifier :: [ModsIdentifier]
        , subject :: [ModsSubject]
}

data ModsRecordInfo = ModsRecordInfo {
        recordContentSource :: T.Text
        , recordIdentifier :: T.Text
}

data ModsOriginInfo = ModsOriginInfo {
        dateIssued :: Maybe Integer
        , publisher :: Maybe T.Text
}

data ModsLanguage = ModsLanguage {
        language :: T.Text
}

data ModsTitleInfo = ModsTitleInfo {
        title :: T.Text
}

data ModsName = ModsName {
        nameType :: T.Text
        , namePart :: [ModsIdentifier]
        , nameRole :: [T.Text]
        , nameIdentifier :: [ModsIdentifier]
}

data ModsIdentifier = ModsIdentifier {
        identifierType :: T.Text
        , identifierValue :: T.Text
}

data ModsSubject = ModsSubject {
        language :: T.Text
        , topic :: T.Text
}

cfclasses :: M.Map T.Text T.Text
cfclasses = M.fromList [
        ("publication/journal-article", "eda2d9e9-34c5-11e1-b86c-0800200c9a66")
        , ("publication/review-article", "eda2d9e9-34c5-11e1-b86c-0800200c9a66")
        , ("publication/editorial-letter", "eda2d9e9-34c5-11e1-b86c-0800200c9a66")
        , ("conference/paper", "eda2d9e9-34c5-11e1-b86c-0800200c9a66")
        , ("conference/proceeeding", "eda2d9e9-34c5-11e1-b86c-0800200c9a66")
        , ("conference/other", "43afa201-2979-42b0-b283-ed609058d90a")
        , ("conference/poster", "43afa201-2979-42b0-b283-ed609058d90a")
        , ("publication/edited-book", "f5e38c52-d56a-4878-879c-31526788b19d")
        , ("publication/book", "2522c045-5090-4da2-824c-583e039e23b3")
        , ("publication/book-chapter", "b7ddff91-81b9-42b1-8228-190329ea6557")
        , ("publication/doctoral-thesis", "eda2d9f1-34c5-11e1-b86c-0800200c9a66")
        , ("publication/licentiate-thesis", "eda2d9f1-34c5-11e1-b86c-0800200c9a66")
        , ("publication/report", "eda2d9f1-34c5-11e1-b86c-0800200c9a66")
        , ("publication/report-chapter", "eda2d9f1-34c5-11e1-b86c-0800200c9a66")
        , ("intellectual-property/patent", "cf7799e3-3477-11e1-b86c-0800200c9a66")
        , ("publication/other", "7eb3f358-bfc1-45d4-9ec6-b16d99f0ded6")
        , ("publication/magazine-article", "d4753dda-e7a0-4837-ae7d-648a8d85b62c")
        , ("publication/newspaper-article", "d4753dda-e7a0-4837-ae7d-648a8d85b62c")]

toCfResPubl :: ModsRecord -> CerifRecord
toCfResPubl cr = CerifRecord {
        resPubl = [CfResPubl {cfResPublId = (recordIdentifier $ recordInfo cr), cfResPublDate = "2017-01-01"}]
        , resPublTitle = []
        , resPublAbstr = []
        , resPublKeyw = []
        , pers = []
        , persName = []
        , persName_Pers = []
        , pers_ResPubl = []
        , orgUnit = []
        , orgUnitName = []
}
