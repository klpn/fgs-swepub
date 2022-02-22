{-# LANGUAGE OverloadedLabels, OverloadedStrings, QuasiQuotes, DuplicateRecordFields, DeriveGeneric, RecordWildCards #-}

module Slupub where

import           Cerif
import           Control.Lens hiding ((.=))
import           Data.Aeson
import           Data.Generics.Labels
import           Data.List
import           Data.Maybe
import           GHC.Generics
import qualified Data.Text as T

data SlupubRecord = SlupubRecord {
        publication :: SluPublication
}
        deriving (Show, Generic)

data SluPublication = SluPublication {
        publId :: T.Text
        , publYear :: T.Text
        , title :: T.Text
        , language :: T.Text
        , abstract :: T.Text
        , keyword :: [T.Text]
}
        deriving (Show)

instance FromJSON SlupubRecord

instance FromJSON SluPublication where
        parseJSON = withObject "slupublication" $ \o -> do
                publIdRaw <- o .: "publ_id"
                publYearRaw <- o .: "publyear"
                title <- o .: "cftitle"
                language <- o .: "language_cgvalue"
                abstract <- o .: "cfabstr"
                keywRaw <- o .: "srckeywords"
                let publId = T.takeWhile (/='.') publIdRaw
                let publYear = T.takeWhile (/='.') publYearRaw
                let keyword = T.splitOn "; " keywRaw
                return SluPublication{..}

slupubToCfResPubl :: SlupubRecord -> CerifRecord
slupubToCfResPubl sr = CerifRecord {
        resPubl = [CfResPubl {cfResPublId = (publId $ publication sr),
                cfResPublDate = (publYear $ publication sr)}]
        , resPublTitle = [CfResPublTitle {cfResPublId = (publId $ publication sr),
                cfLangCode = (language $ publication sr), 
                cfTrans = "o", cfTitle = (title $ publication sr)}]
        , resPublAbstr = [CfResPublAbstr {cfResPublId = (publId $ publication sr),
                cfLangCode = (language $ publication sr), 
                cfTrans = "o", cfAbstr = (abstract $ publication sr)}]
        , resPublKeyw = []
        , pers = []
        , persName = []
        , persName_Pers = []
        , pers_ResPubl = []
        , orgUnit = []
        , orgUnitName = []
}
