{-# LANGUAGE OverloadedLabels, OverloadedStrings, QuasiQuotes, DuplicateRecordFields, DeriveGeneric, RecordWildCards #-}

module Swepub where

import           Cerif
import           Control.Lens
import           Data.Aeson
import           Data.Generics.Labels
import           Data.List
import           Data.Maybe
import           GHC.Generics
import qualified Data.Text as T

data SwepubRecord = SwepubRecord {
        swepubId :: T.Text
        , identifiedBy :: [Identifier]
        , instanceOf :: SwepubInstance
        , publication :: [Publication]
}
        deriving (Show)

data SwepubInstance = SwepubInstance {
        contribution :: [Contribution]
        , title :: [Title]
        , language :: [Language]
        , summary :: [Summary]
        , subject :: [Subject]
}
        deriving (Show, Generic)

data Identifier = Identifier {
        identifierType :: T.Text
        , identifierValue :: T.Text
        , identifierSource :: Maybe Source
}
        deriving (Show)

data Contribution = Contribution {
        agent :: Agent 
        , affiliation :: Maybe [Affiliation]
        , role :: [Role]
}
        deriving (Show, Generic)

data Agent = 
        Person {familyName :: T.Text, givenName :: T.Text, identifiedBy :: [Identifier]}
        | Organization {name :: T.Text, identifiedBy :: [Identifier]}
        deriving (Show, Generic)

data Title = Title {
        mainTitle :: T.Text
}
        deriving (Show, Generic)

data Source = Source {
        code :: T.Text
}
        deriving (Show, Generic)

data Publication = Publication {
        date :: T.Text
}
        deriving (Show, Generic)

data Language = Language {
        code :: T.Text
}
        deriving (Show, Generic)

data Role = Role {
        roleid :: T.Text
}
        deriving (Show)

data Summary = Summary {
        label :: T.Text
}
        deriving (Show, Generic)

data Subject = Subject {
       subjcode :: T.Text
        , prefLabel :: T.Text
        , language :: Language
}
        deriving (Show, Generic)

data Affiliation = Affiliation {
        name :: T.Text
        , language :: Language
        , identifiedBy :: [Identifier] 
        , affiliation :: Maybe [Affiliation]
}
        deriving (Show, Generic)

instance FromJSON SwepubRecord where
        parseJSON = withObject "swepubrecord" $ \o -> do
                swepubId <- o .: "@id"
                identifiedBy <- o .: "identifiedBy"
                instanceOf <- o .: "instanceOf"
                publication <- o .: "publication"
                return SwepubRecord{..}

instance FromJSON SwepubInstance where
        parseJSON = withObject "swepubinstance" $ \o -> do
                contribution <- o .: "contribution"
                title <- o .: "hasTitle"
                language <- o .: "language"
                summary <- o .: "summary"
                subject <- o .: "subject"
                return SwepubInstance{..}

instance FromJSON Identifier where
        parseJSON = withObject "identifier" $ \o -> do
                identifierType <- o .: "@type"
                identifierValue <- o .: "value"
                identifierSource <- o .:? "source"
                return Identifier{..}

instance FromJSON Contribution where
        parseJSON = withObject "contribution" $ \o -> do
                agent <- o .: "agent"
                affiliation <- o .:? "hasAffiliation"
                role <- o .: "role"
                return Contribution{..}

instance FromJSON Language where
        parseJSON = withObject "language" $ \o -> do
                code <- o .: "code"
                return Language{..}

instance FromJSON Role where
        parseJSON = withObject "role" $ \o -> do
                roleid <- o .: "@id"
                return Role{..}

instance FromJSON Subject where
        parseJSON = withObject "subject" $ \o -> do
                subjcode <- o .: "code"
                prefLabel <- o .: "prefLabel"
                language <- o .: "language"
                return Subject{..}

instance FromJSON Agent where
        parseJSON = genericParseJSON swepubOptions

instance FromJSON Title
instance FromJSON Source
instance FromJSON Publication
instance FromJSON Summary

instance FromJSON Affiliation where
        parseJSON = withObject "affiliation" $ \o -> do
                name <- o .: "name"
                language <- o .: "language"
                identifiedBy <- o .: "identifiedBy"
                affiliation <- o .:? "hasAffiliation"
                return Affiliation{..}

swepubOptions = defaultOptions {sumEncoding = defaultTaggedObject{tagFieldName="@type"}}

toCfResPubl :: SwepubRecord -> CerifRecord
toCfResPubl sr = CerifRecord {
        resPubl = CfResPubl {cfResPublId = (swepubId sr), cfResPublDate = (dates sr !! 0)}
        , resPublTitle = titles sr
        , resPublAbstr = abstrs sr
        , resPublKeyw = keyws sr
        , pers = persons sr
        , persName = persnames sr
        , persName_Pers = persnames_pers sr
        , pers_ResPubl = pers_respubl sr
        , orgUnit = ous sr
        , orgUnitName = ounames sr
}

dates :: SwepubRecord -> [T.Text]
dates sr =  map (\p -> T.pack $ T.unpack(date p) ++ "-01-01") (publication sr)

titles :: SwepubRecord -> [CfResPublTitle]
titles sr = map (\t -> toCfResPublTitle sr t) (title $ instanceOf sr)

abstrs :: SwepubRecord -> [CfResPublAbstr]
abstrs sr = map (\s -> toCfResPublAbstr sr s) (summary $ instanceOf sr)

keyws :: SwepubRecord -> [CfResPublKeyw]
keyws sr = map (\s -> toCfResPublKeyw sr s) (subject $ instanceOf sr)

persons :: SwepubRecord -> [CfPers]
persons sr = map toCfPers [a | a@(Person {}) <- agents]
        where
                agents = map agent (contribution $ instanceOf sr)

persnames :: SwepubRecord -> [CfPersName]
persnames sr = map toCfPersName [a | a@(Person {}) <- agents]
        where
                agents = map agent (contribution $ instanceOf sr)

persnames_pers :: SwepubRecord -> [CfPersName_Pers]
persnames_pers sr = map toCfPersName_Pers [a | a@(Person {}) <- agents]
        where
                agents = map agent (contribution $ instanceOf sr)

pers_respubl :: SwepubRecord -> [CfPers_ResPubl]
pers_respubl sr = concat $ map (\c -> pers_respublContr sr c) [c | c <- contrs, isPerson (agent c)]
        where
                contrs = contribution $ instanceOf sr

isPerson :: Agent -> Bool
isPerson Person {} = True
isPerson Organization {} = False

pers_respublContr :: SwepubRecord -> Contribution -> [CfPers_ResPubl]
pers_respublContr sr c = map (\r -> toCfPers_ResPubl sr (agent c) r) (role c)

ous :: SwepubRecord -> [CfOrgUnit]
ous sr = nub $ map toCfOrgUnit (affils sr)

ounames :: SwepubRecord -> [CfOrgUnitName]
ounames sr = nub $ map toCfOrgUnitName (affils sr)

affils :: SwepubRecord -> [Affiliation]
affils sr = concat (map (fromMaybe []) affs)
        where
                affs = concat [mainaffs, subaffs]
                mainaffs = map (\a -> a ^. #affiliation) (contribution $ instanceOf sr)
                subaffs = map (\a -> a ^. #affiliation) (concat (map (fromMaybe []) mainaffs))

toCfResPublTitle :: SwepubRecord -> Title -> CfResPublTitle
toCfResPublTitle sr t =
        CfResPublTitle {cfResPublId = (swepubId sr), cfLangCode = l, cfTrans = "o", cfTitle = mt}
                where
                        l0 = ((instanceOf sr) ^. #language) !! 0
                        l = l0 ^. #code
                        mt = mainTitle t

toCfResPublAbstr :: SwepubRecord -> Summary -> CfResPublAbstr
toCfResPublAbstr sr s =
        CfResPublAbstr {cfResPublId = (swepubId sr), cfLangCode = l, cfTrans = "o", cfAbstr = sl}
                where
                        l0 = ((instanceOf sr) ^. #language) !! 0
                        l = l0 ^. #code
                        sl = label s

toCfResPublKeyw :: SwepubRecord -> Subject -> CfResPublKeyw
toCfResPublKeyw sr s =
        CfResPublKeyw {cfResPublId = (swepubId sr), cfLangCode = l, cfTrans = "o", cfKeyw = pl}
                where
                        l = s ^. #language . #code
                        pl = prefLabel s

toCfPers :: Agent -> CfPers
toCfPers a = CfPers {cfPersId = (identifierValue $ (a ^. #identifiedBy) !! 0)}

toCfPersName :: Agent -> CfPersName
toCfPersName a = CfPersName {
        cfPersNameId = T.pack $ (T.unpack $ identifierValue $ (a ^. #identifiedBy) !! 0) ++ "-N"
        , cfFamilyNames = familyName a
        , cfFirstNames = givenName a
}

toCfPersName_Pers :: Agent -> CfPersName_Pers
toCfPersName_Pers a = CfPersName_Pers {
        cfPersId = (identifierValue $ (a ^. #identifiedBy) !! 0)
        , cfPersNameId = T.pack $ (T.unpack $ identifierValue $ (a ^. #identifiedBy) !! 0) ++ "-N"
        , cfClassId = "SwepubName"
        , cfClassSchemeId = "FGS_Swepub"
        , cfStartDate = "1900-01-01T00:00:00"
        , cfEndDate = "2099-12-31T00:00:00"
}

toCfPers_ResPubl :: SwepubRecord -> Agent -> Role -> CfPers_ResPubl
toCfPers_ResPubl sr a r = CfPers_ResPubl {
        cfPersId = (identifierValue $ (a ^. #identifiedBy) !! 0)
        , cfResPublId = swepubId sr
        , cfClassId = roleid r
        , cfClassSchemeId = "FGS_Swepub"
        , cfStartDate = "1900-01-01T00:00:00"
        , cfEndDate = "2099-12-31T00:00:00"
}

toCfOrgUnit :: Affiliation -> CfOrgUnit
toCfOrgUnit a = CfOrgUnit {cfOrgUnitId = (identifierValue $ (a ^. #identifiedBy) !! 0)}

toCfOrgUnitName :: Affiliation -> CfOrgUnitName
toCfOrgUnitName a = CfOrgUnitName {
        cfOrgUnitId = (identifierValue $ (a ^. #identifiedBy) !! 0)
        , cfLangCode = l
        , cfTrans = "o"
        , cfName = n
}
        where
                l = a ^. #language . #code
                n = a ^. #name
