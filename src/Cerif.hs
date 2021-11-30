{-# LANGUAGE OverloadedLabels, OverloadedStrings, QuasiQuotes, DuplicateRecordFields, DeriveGeneric, RecordWildCards #-}

module Cerif where

import           Control.Lens
import           Data.Generics.Labels
import           GHC.Generics
import qualified Data.Text as T

data CerifRecord = CerifRecord {
        resPubl :: [CfResPubl]
        , resPublTitle :: [CfResPublTitle]
        , resPublAbstr :: [CfResPublAbstr]
        , resPublKeyw :: [CfResPublKeyw]
        , pers :: [CfPers]
        , persName :: [CfPersName]
        , persName_Pers :: [CfPersName_Pers]
        , pers_ResPubl :: [CfPers_ResPubl]
        , orgUnit :: [CfOrgUnit]
        , orgUnitName :: [CfOrgUnitName]
}
        deriving (Eq, Show, Generic)

data CfResPubl = CfResPubl {
        cfResPublId :: T.Text
        , cfResPublDate :: T.Text
}
        deriving (Eq, Show, Generic)

data CfResPublTitle = CfResPublTitle {
        cfResPublId :: T.Text
        , cfLangCode :: T.Text
        , cfTrans :: T.Text
        , cfTitle :: T.Text
}
        deriving (Eq, Show, Generic)

data CfResPublAbstr = CfResPublAbstr {
        cfResPublId :: T.Text
        , cfLangCode :: T.Text
        , cfTrans :: T.Text
        , cfAbstr :: T.Text
}
        deriving (Eq, Show, Generic)

data CfResPublKeyw = CfResPublKeyw {
        cfResPublId :: T.Text
        , cfLangCode :: T.Text
        , cfTrans :: T.Text
        , cfKeyw :: T.Text
}
        deriving (Eq, Show, Generic)

data CfPers = CfPers {
        cfPersId :: T.Text
}
        deriving (Eq, Show, Generic)

data CfPersName = CfPersName {
        cfPersNameId :: T.Text
        , cfFamilyNames :: T.Text
        , cfFirstNames :: T.Text
}
        deriving (Eq, Show, Generic)

data CfPersName_Pers = CfPersName_Pers {
        cfPersNameId :: T.Text
        , cfPersId :: T.Text
        , cfClassId :: T.Text
        , cfClassSchemeId :: T.Text
        , cfStartDate :: T.Text
        , cfEndDate :: T.Text
}
        deriving (Eq, Show, Generic)

data CfPers_ResPubl = CfPers_ResPubl {
        cfPersId :: T.Text
        , cfResPublId :: T.Text
        , cfClassId :: T.Text
        , cfClassSchemeId :: T.Text
        , cfStartDate :: T.Text
        , cfEndDate :: T.Text
}
        deriving (Eq, Show, Generic)

data CfOrgUnit = CfOrgUnit {
        cfOrgUnitId :: T.Text
}
        deriving (Eq, Show, Generic)

data CfOrgUnitName = CfOrgUnitName {
        cfOrgUnitId :: T.Text
        , cfLangCode :: T.Text
        , cfTrans :: T.Text
        , cfName :: T.Text
}
        deriving (Eq, Show, Generic)
