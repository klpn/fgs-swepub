{-# LANGUAGE OverloadedLabels, OverloadedStrings, QuasiQuotes, DuplicateRecordFields, DeriveGeneric, RecordWildCards #-}

module Slupub where

import           Cerif
import           Isolang
import           Control.Lens hiding ((.=))
import           Data.Aeson
import           Data.Generics.Labels
import           Data.List
import           Data.Maybe
import           GHC.Generics
import qualified Data.Map.Strict as M
import qualified Data.Text as T

data SlupubRecord = SlupubRecord {
        publication :: SluPublication
}
        deriving (Show, Generic)

instance FromJSON SlupubRecord

data SluPublication = SluPublication {
        publId :: T.Text
        , publYear :: T.Text
        , title :: T.Text
        , language :: T.Text
        , abstract :: T.Text
        , keyword :: [T.Text]
        , card :: [SluCard]
}
        deriving (Show)

instance FromJSON SluPublication where
        parseJSON = withObject "slupublication" $ \o -> do
                publIdRaw <- o .: "publ_id"
                publYearRaw <- o .: "publyear"
                title <- o .: "cftitle"
                languageRaw <- o .: "language_cgvalue"
                abstract <- o .: "cfabstr"
                card <- o .: "related_cards"
                keywRaw <- o .: "srckeywords"
                let publId = T.takeWhile (/='.') publIdRaw
                let publYear = T.takeWhile (/='.') publYearRaw
                let keyword = T.splitOn "; " keywRaw
                let language = fromMaybe "UNK" $ M.lookup languageRaw isolang 
                return SluPublication{..}

data SluCard = SluCard {
        person :: [SluPers]
        , organisation :: [SluOrg]
}
        deriving (Show)

instance FromJSON SluCard where
        parseJSON = withObject "slucard" $ \o -> do
                person <- o .: "related_persons"
                organisation <- o .: "related_organisations"
                return SluCard{..}

data SluPers = SluPers {
        familyName :: T.Text
        , givenName :: T.Text
        , orcid :: T.Text
}
        deriving (Show)

instance FromJSON SluPers where
        parseJSON = withObject "sluperson" $ \o -> do
                familyName <- o .: "cffamilynames"
                givenName <- o .: "cffirstnames"
                orcid <- o .: "orcid"
                return SluPers{..}

data SluOrg = SluOrg {
        orgShortDescr :: T.Text
        , oid :: T.Text
}
        deriving (Show)

instance FromJSON SluOrg where
        parseJSON = withObject "sluorg" $ \o -> do
                orgShortDescr <- o .: "short_description__1"
                oid <- o .: "oid"
                return SluOrg{..}

toCfResPubl :: SlupubRecord -> CerifRecord
toCfResPubl sr = CerifRecord {
        resPubl = [CfResPubl {cfResPublId = (publId $ publication sr),
                cfResPublDate = T.concat[(publYear $ publication sr), "-01-01"]}]
        , resPublTitle = [CfResPublTitle {cfResPublId = (publId $ publication sr),
                cfLangCode = (language $ publication sr), 
                cfTrans = "o", cfTitle = (title $ publication sr)}]
        , resPublAbstr = [CfResPublAbstr {cfResPublId = (publId $ publication sr),
                cfLangCode = (language $ publication sr), 
                cfTrans = "o", cfAbstr = (abstract $ publication sr)}]
        , resPublKeyw = keyws sr
        , pers = persons sr
        , persName = persnames sr
        , persName_Pers = persnames_pers sr 
        , pers_ResPubl = pers_respubl sr
        , orgUnit = ous sr
        , orgUnitName = ounames sr
}

keyws :: SlupubRecord -> [CfResPublKeyw]
keyws sr = (\s -> toCfResPublKeyw sr s) <$> (keyword $ publication sr)

persons :: SlupubRecord -> [CfPers]
persons sr = nub $ toCfPers <$> concat (person <$> (card $ publication sr))

persnames :: SlupubRecord -> [CfPersName]
persnames sr = nub $ toCfPersName <$> concat (person <$> (card $ publication sr))

persnames_pers :: SlupubRecord -> [CfPersName_Pers]
persnames_pers sr = nub $ toCfPersName_Pers <$> concat (person <$> (card $ publication sr))

pers_respubl :: SlupubRecord -> [CfPers_ResPubl]
pers_respubl sr = nub $ (\s -> toCfPers_ResPubl (publication sr) s) <$>
        concat (person <$> (card $ publication sr))

ous :: SlupubRecord -> [CfOrgUnit]
ous sr = nub $ toCfOrgUnit <$> concat (organisation <$> (card $ publication sr))

ounames :: SlupubRecord -> [CfOrgUnitName]
ounames sr = nub $ (\s -> toCfOrgUnitName (publication sr) s) <$>
        concat (organisation <$> (card $ publication sr))

toCfResPublKeyw :: SlupubRecord -> T.Text -> CfResPublKeyw
toCfResPublKeyw sr s =
        CfResPublKeyw {cfResPublId = (publId $ publication sr),
                cfLangCode = (language $ publication sr), cfTrans = "o", cfKeyw = s}

toCfPers :: SluPers -> CfPers
toCfPers s = CfPers {cfPersId = orcid s}

toCfPersName :: SluPers -> CfPersName
toCfPersName s = CfPersName {
        cfPersNameId = T.concat [(orcid s), "-N"]
        , cfFamilyNames = familyName s
        , cfFirstNames = givenName s
}

toCfPersName_Pers :: SluPers -> CfPersName_Pers
toCfPersName_Pers s = CfPersName_Pers {
        cfPersId = orcid s
        , cfPersNameId = T.concat [(orcid s), "-N"]
        , cfClassId = "SlupubName"
        , cfClassSchemeId = "FGS_Swepub"
        , cfStartDate = "1900-01-01T00:00:00"
        , cfEndDate = "2099-12-31T00:00:00"
}

toCfPers_ResPubl :: SluPublication -> SluPers -> CfPers_ResPubl
toCfPers_ResPubl p s = CfPers_ResPubl {
        cfPersId = orcid s
        , cfResPublId = publId p
        , cfClassId = "SlupubAuthor"
        , cfClassSchemeId = "FGS_Swepub"
        , cfStartDate = "1900-01-01T00:00:00"
        , cfEndDate = "2099-12-31T00:00:00"
}

toCfOrgUnit :: SluOrg -> CfOrgUnit
toCfOrgUnit s = CfOrgUnit {cfOrgUnitId = oid s}

toCfOrgUnitName :: SluPublication -> SluOrg -> CfOrgUnitName
toCfOrgUnitName p s = CfOrgUnitName {
        cfOrgUnitId = oid s
        , cfLangCode = language p
        , cfTrans = "o"
        , cfName = orgShortDescr s
}
