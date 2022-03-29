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
        originInfo :: [ModsOriginInfo]
        , name :: [ModsName]
        , abstract :: [T.Text]
        , identifier :: [ModsIdentifier]
        , subject :: [ModsSubject]
}

data ModsOriginInfo = ModsOriginInfo {
        dateIssued :: Maybe Integer
        , publisher :: Maybe T.Text
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

toCfResPubl :: ModsRecord -> CerifRecord
toCfResPubl cr = CerifRecord {
        resPubl = []
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
