{-# LANGUAGE OverloadedLabels, OverloadedStrings, QuasiQuotes, DuplicateRecordFields, DeriveGeneric, RecordWildCards #-}

module Swepub where

import           Cerif
import           Control.Lens hiding ((.=))
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
        deriving (Show, Generic)

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
        deriving (Show, Generic)

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

instance ToJSON SwepubRecord where
        toJSON SwepubRecord{..} = object [
                "@context" .= T.pack "https://id.kb.se/context.jsonld"
                , "@id" .= swepubId
                , "@type" .= T.pack "Instance"
                , "instanceOf" .= instanceOf]

instance FromJSON SwepubInstance where
        parseJSON = withObject "swepubinstance" $ \o -> do
                contribution <- o .: "contribution"
                title <- o .: "hasTitle"
                language <- o .: "language"
                summary <- o .: "summary"
                subject <- o .: "subject"
                return SwepubInstance{..}

instance ToJSON SwepubInstance

instance FromJSON Identifier where
        parseJSON = withObject "identifier" $ \o -> do
                identifierType <- o .: "@type"
                identifierValue <- o .: "value"
                identifierSource <- o .:? "source"
                return Identifier{..}

instance ToJSON Identifier

instance FromJSON Contribution where
        parseJSON = withObject "contribution" $ \o -> do
                agent <- o .: "agent"
                affiliation <- o .:? "hasAffiliation"
                role <- o .: "role"
                return Contribution{..}

instance ToJSON Contribution

instance FromJSON Language where
        parseJSON = withObject "language" $ \o -> do
                code <- o .: "code"
                return Language{..}

instance ToJSON Language

instance FromJSON Role where
        parseJSON = withObject "role" $ \o -> do
                roleid <- o .: "@id"
                return Role{..}

instance ToJSON Role

instance FromJSON Subject where
        parseJSON = withObject "subject" $ \o -> do
                subjcode <- o .: "code"
                prefLabel <- o .: "prefLabel"
                language <- o .: "language"
                return Subject{..}

instance ToJSON Subject

instance FromJSON Agent where
        parseJSON = genericParseJSON swepubOptions

instance ToJSON Agent

instance FromJSON Title
instance ToJSON Title 
instance FromJSON Source
instance ToJSON Source
instance FromJSON Publication
instance FromJSON Summary
instance ToJSON Summary

instance FromJSON Affiliation where
        parseJSON = withObject "affiliation" $ \o -> do
                name <- o .: "name"
                language <- o .: "language"
                identifiedBy <- o .: "identifiedBy"
                affiliation <- o .:? "hasAffiliation"
                return Affiliation{..}

instance ToJSON Affiliation

swepubOptions = defaultOptions {sumEncoding = defaultTaggedObject{tagFieldName="@type"}}

toCfResPubl :: SwepubRecord -> CerifRecord
toCfResPubl sr = CerifRecord {
        resPubl = [CfResPubl {cfResPublId = (swepubId sr), cfResPublDate = (dates sr !! 0)}]
        , resPublTitle = titles sr
        , resPublAbstr = abstrs sr
        , resPublKeyw = keyws sr
        , resPubl_Class = []
        , pers = persons sr
        , persName = persnames sr
        , persName_Pers = persnames_pers sr
        , pers_ResPubl = pers_respubl sr
        , orgUnit = ous sr
        , orgUnitName = ounames sr
}

dates :: SwepubRecord -> [T.Text]
dates sr = (\p -> T.concat [(date p), "-01-01"]) <$> (publication sr)

titles :: SwepubRecord -> [CfResPublTitle]
titles sr = (\t -> toCfResPublTitle sr t) <$> (title $ instanceOf sr)

abstrs :: SwepubRecord -> [CfResPublAbstr]
abstrs sr = (\s -> toCfResPublAbstr sr s) <$> (summary $ instanceOf sr)

keyws :: SwepubRecord -> [CfResPublKeyw]
keyws sr = (\s -> toCfResPublKeyw sr s) <$> (subject $ instanceOf sr)

persons :: SwepubRecord -> [CfPers]
persons sr = toCfPers <$> [a | a@(Person {}) <- agents]
        where
                agents = agent <$> (contribution $ instanceOf sr)

persnames :: SwepubRecord -> [CfPersName]
persnames sr = toCfPersName <$> [a | a@(Person {}) <- agents]
        where
                agents = agent <$> (contribution $ instanceOf sr)

persnames_pers :: SwepubRecord -> [CfPersName_Pers]
persnames_pers sr = toCfPersName_Pers <$> [a | a@(Person {}) <- agents]
        where
                agents = agent <$> (contribution $ instanceOf sr)

pers_respubl :: SwepubRecord -> [CfPers_ResPubl]
pers_respubl sr = concat $ (\c -> pers_respublContr sr c) <$> [c | c <- contrs, isPerson (agent c)]
        where
                contrs = contribution $ instanceOf sr

isPerson :: Agent -> Bool
isPerson Person {} = True
isPerson Organization {} = False

pers_respublContr :: SwepubRecord -> Contribution -> [CfPers_ResPubl]
pers_respublContr sr c = (\r -> toCfPers_ResPubl sr (agent c) r) <$> (role c)

ous :: SwepubRecord -> [CfOrgUnit]
ous sr = nub $ toCfOrgUnit <$> (affils sr)

ounames :: SwepubRecord -> [CfOrgUnitName]
ounames sr = nub $ toCfOrgUnitName <$> (affils sr)

affils :: SwepubRecord -> [Affiliation]
affils sr = concat ((fromMaybe []) <$> affs)
        where
                affs = concat [mainaffs, subaffs]
                mainaffs = (\a -> a ^. #affiliation) <$> (contribution $ instanceOf sr)
                subaffs = (\a -> a ^. #affiliation) <$> (concat ((fromMaybe []) <$> mainaffs))

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
        cfPersNameId = T.concat [(identifierValue $ (a ^. #identifiedBy) !! 0), "-N"]
        , cfFamilyNames = familyName a
        , cfFirstNames = givenName a
}

toCfPersName_Pers :: Agent -> CfPersName_Pers
toCfPersName_Pers a = CfPersName_Pers {
        cfPersId = (identifierValue $ (a ^. #identifiedBy) !! 0)
        , cfPersNameId = T.concat [(identifierValue $ (a ^. #identifiedBy) !! 0), "-N"]
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

toSwepubRecords :: CerifRecord -> [SwepubRecord]
toSwepubRecords cr = (\crpu -> toSwepubRecord cr crpu) <$> cr ^. #resPubl

toSwepubRecord :: CerifRecord -> CfResPubl -> SwepubRecord
toSwepubRecord cr crpu = SwepubRecord {
        swepubId = crpu ^. #cfResPublId
        , identifiedBy = []
        , instanceOf = toSwepubInstance cr crpu
        , publication = []
}

toSwepubInstance :: CerifRecord -> CfResPubl -> SwepubInstance
toSwepubInstance cr crpu = SwepubInstance {
        contribution = []
        , title = toSwepubTitle cr crpu
        , language = toSwepubLanguage cr crpu
        , summary = toSwepubSummary cr crpu
        , subject = toSwepubSubject cr crpu
}

toSwepubSummary :: CerifRecord -> CfResPubl -> [Summary]
toSwepubSummary cr crpu = (\a -> Summary {label = a ^. #cfAbstr}) <$>
        [a | a <- (cr ^. #resPublAbstr), a ^. #cfResPublId == crpu ^. #cfResPublId]

toSwepubTitle :: CerifRecord -> CfResPubl -> [Title]
toSwepubTitle cr crpu = (\a -> Title {mainTitle = a ^. #cfTitle}) <$>
        [a | a <- (cr ^. #resPublTitle), a ^. #cfResPublId == crpu ^. #cfResPublId]

toSwepubLanguage :: CerifRecord -> CfResPubl -> [Language]
toSwepubLanguage cr crpu = (\a -> Language {code = a ^. #cfLangCode}) <$>
        [a | a <- (cr ^. #resPublTitle), a ^. #cfResPublId == crpu ^. #cfResPublId]

toSwepubSubject :: CerifRecord -> CfResPubl -> [Subject]
toSwepubSubject cr crpu = subj <$>
        [a | a <- (cr ^. #resPublKeyw), a ^. #cfResPublId == crpu ^. #cfResPublId]

subj :: CfResPublKeyw -> Subject
subj k = Subject {
        subjcode = k ^. #cfKeyw
        , language = Language {code = k ^. #cfLangCode}
        , prefLabel = k ^. #cfKeyw
}
