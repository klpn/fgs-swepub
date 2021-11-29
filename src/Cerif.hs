{-# LANGUAGE OverloadedLabels, OverloadedStrings, QuasiQuotes, DuplicateRecordFields, DeriveGeneric, RecordWildCards #-}

module Cerif where

import           Control.Lens
import           Data.Generics.Labels
import           GHC.Generics
import qualified Data.Text as T

data CerifRecord = CerifRecord {
        resPubl :: CfResPubl
        , resPublTitle :: [CfResPublTitle]
        , resPublAbstr :: [CfResPublAbstr]
        , resPublKeyw :: [CfResPublKeyw]
        , pers :: [CfPers]
        , persName :: [CfPersName]
        , persName_Pers :: [CfPersName_Pers]
        , persResPubl :: [CfPers_ResPubl]
        , orgUnit :: [CfOrgUnit]
        , orgUnitName :: [CfOrgUnitName]
}
        deriving (Show, Generic)

data CfResPubl = CfResPubl {
        cfResPublId :: T.Text
        , cfResPublDate :: T.Text
}
        deriving (Show, Generic)

data CfResPublTitle = CfResPublTitle {
        cfResPublId :: T.Text
        , cfLangCode :: T.Text
        , cfTrans :: T.Text
        , cfTitle :: T.Text
}
        deriving (Show, Generic)

data CfResPublAbstr = CfResPublAbstr {
        cfResPublId :: T.Text
        , cfLangCode :: T.Text
        , cfTrans :: T.Text
        , cfAbstr :: T.Text
}
        deriving (Show, Generic)

data CfResPublKeyw = CfResPublKeyw {
        cfResPublId :: T.Text
        , cfLangCode :: T.Text
        , cfTrans :: T.Text
        , cfKeyw :: T.Text
}
        deriving (Show, Generic)

data CfPers = CfPers {
        cfPersId :: T.Text
}
        deriving (Show)

data CfPersName = CfPersName {
        cfPersNameId :: T.Text
        , cfFamilyNames :: T.Text
        , cfFirstNames :: T.Text
}
        deriving (Show)

data CfPersName_Pers = CfPersName_Pers {
        cfPersId :: T.Text
        , cfPersNameId :: T.Text
        , cfClassId :: T.Text
        , cfClassSchemeId :: T.Text
        , cfStartDate :: T.Text
        , cfEndDate :: T.Text
}
        deriving (Show)

data CfPers_ResPubl = CfPers_ResPubl {
        cfPersId :: T.Text
        , cfResPublId :: T.Text
        , cfClassId :: T.Text
        , cfClassSchemeId :: T.Text
        , cfStartDate :: T.Text
        , cfEndDate :: T.Text
}
        deriving (Show)

data CfOrgUnit = CfOrgUnit {
        cfOrgUnitId :: T.Text
}
        deriving (Eq, Show)

data CfOrgUnitName = CfOrgUnitName {
        cfOrgUnitId :: T.Text
        , cfLangCode :: T.Text
        , cfTrans :: T.Text
        , cfName :: T.Text
}
        deriving (Eq, Show)
