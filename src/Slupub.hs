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
        , publType :: T.Text
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
                publType <- o .: "publication_type_cgvalue"
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

cfclasses :: M.Map T.Text T.Text
cfclasses = M.fromList [
        ("Journal article", "eda2d9e9-34c5-11e1-b86c-0800200c9a66")
        , ("Journal article review", "eda2d9e9-34c5-11e1-b86c-0800200c9a66")
        , ("editorial", "eda2d9e9-34c5-11e1-b86c-0800200c9a66")
        , ("letter", "eda2d9e9-34c5-11e1-b86c-0800200c9a66")
        , ("data_paper", "eda2d9e9-34c5-11e1-b86c-0800200c9a66")
        , ("other_sci", "eda2d9e9-34c5-11e1-b86c-0800200c9a66")
        , ("Conference proceedings article", "eda2d9e9-34c5-11e1-b86c-0800200c9a66")
        , ("Conference proceeeding", "eda2d9e9-34c5-11e1-b86c-0800200c9a66")
        , ("conference_abstract", "154e80ab-e825-4f7c-9430-bdf7ee971425")
        , ("conference_poster", "93aa5afc-19e5-4995-99b5-47ee8c80b3fc")
        , ("conference_other", "43afa201-2979-42b0-b283-ed609058d90a")
        , ("Edited book", "f5e38c52-d56a-4878-879c-31526788b19d")
        , ("Authored book", "2522c045-5090-4da2-824c-583e039e23b3")
        , ("Inbook", "b7ddff91-81b9-42b1-8228-190329ea6557")
        , ("Doctoral thesis", "eda2d9f1-34c5-11e1-b86c-0800200c9a66")
        , ("Licentiate thesis", "eda2d9f1-34c5-11e1-b86c-0800200c9a66")
        , ("Report", "eda2d9f1-34c5-11e1-b86c-0800200c9a66")
        , ("Report chapter", "eda2d9f1-34c5-11e1-b86c-0800200c9a66")
        , ("Patent", "cf7799e3-3477-11e1-b86c-0800200c9a66")
        , ("Other", "7eb3f358-bfc1-45d4-9ec6-b16d99f0ded6")
        , ("magazine_article", "d4753dda-e7a0-4837-ae7d-648a8d85b62c")
        , ("newspaper_article", "d4753dda-e7a0-4837-ae7d-648a8d85b62c")]

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
        , resPubl_Class = []
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
