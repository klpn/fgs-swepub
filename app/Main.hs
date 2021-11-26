{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, DeriveGeneric, RecordWildCards #-}

module Main where

import           Data.Aeson
import           Data.List
import           Data.Maybe
import           GHC.Generics
import           System.Environment
import           System.Exit
import           System.IO (hPutStrLn, stderr)
import qualified Data.ByteString.Lazy as L

-- CERIF entities

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
        deriving (Show)

data CfResPubl = CfResPubl {
        cfResPublId :: String
}
        deriving (Show)

data CfResPublTitle = CfResPublTitle {
        cfResPublId :: String
        , cfLangCode :: String
        , cfTrans :: String
        , cfTitle :: String
}
        deriving (Show)

data CfResPublAbstr = CfResPublAbstr {
        cfResPublId :: String
        , cfLangCode :: String
        , cfTrans :: String
        , cfAbstr :: String
}
        deriving (Show)

data CfResPublKeyw = CfResPublKeyw {
        cfResPublId :: String
        , cfLangCode :: String
        , cfTrans :: String
        , cfKeyw :: String
}
        deriving (Show)

data CfPers = CfPers {
        cfPersId :: String
}
        deriving (Show)

data CfPersName = CfPersName {
        cfPersNameId :: String
        , cfFamilyNames :: String
        , cfFirstNames :: String
}
        deriving (Show)

data CfPersName_Pers = CfPersName_Pers {
        cfPersId :: String
        , cfPersNameId :: String
        , cfClassId :: String
        , cfClassSchemeId :: String
        , cfStartDate :: String
        , cfEndDate :: String
}
        deriving (Show)

data CfPers_ResPubl = CfPers_ResPubl {
        cfPersId :: String
        , cfResPublId :: String
        , cfClassId :: String
        , cfClassSchemeId :: String
        , cfStartDate :: String
        , cfEndDate :: String
}
        deriving (Show)

data CfOrgUnit = CfOrgUnit {
        cfOrgUnitId :: String
}
        deriving (Eq, Show)

data CfOrgUnitName = CfOrgUnitName {
        cfOrgUnitId :: String
        , cfLangCode :: String
        , cfTrans :: String
        , cfName :: String
}
        deriving (Eq, Show)

-- Swepub BIBFRAME entities
 
data SwepubRecord = SwepubRecord {
        swepubId :: String
        , sridentifiedBy :: [Identifier]
        , instanceOf :: SwepubInstance
        , publication :: [Publication]
}
        deriving (Show)

data SwepubInstance = SwepubInstance {
        contribution :: [Contribution]
        , title :: [Title]
        , instlanguage :: [Language]
        , summary :: [Summary]
        , subject :: [Subject]
}
        deriving (Show)

data Identifier = Identifier {
        identifierType :: String
        , identifierValue :: String
        , identifierSource :: Maybe Source
}
        deriving (Show)

data Contribution = Contribution {
        agent :: Agent 
        , affiliation :: Maybe [Affiliation]
        , role :: [Role]
}
        deriving (Show)

data Agent = 
        Person {familyName :: String, givenName :: String, identifiedBy :: [Identifier]}
        | Organization {name :: String, identifiedBy :: [Identifier]}
        deriving (Show, Generic)

data Title = Title {
        mainTitle :: String
}
        deriving (Show, Generic)

data Source = Source {
        code :: String
}
        deriving (Show, Generic)

data Publication = Publication {
        date :: String
}
        deriving (Show, Generic)

data Language = Language {
        langcode :: String
}
        deriving (Show)

data Role = Role {
        roleid :: String
}
        deriving (Show)

data Summary = Summary {
        label :: String
}
        deriving (Show, Generic)

data Subject = Subject {
       subjcode :: String
        , prefLabel :: String
        , subjlanguage :: Language
}
        deriving (Show, Generic)

data Affiliation = Affiliation {
        affname :: String
        , afflanguage :: Language
        , affidentifiedBy :: [Identifier] 
        , affaffiliation :: Maybe [Affiliation]
}
        deriving (Show)

instance FromJSON SwepubRecord where
        parseJSON = withObject "swepubrecord" $ \o -> do
                swepubId <- o .: "@id"
                sridentifiedBy <- o .: "identifiedBy"
                instanceOf <- o .: "instanceOf"
                publication <- o .: "publication"
                return SwepubRecord{..}

instance FromJSON SwepubInstance where
        parseJSON = withObject "swepubinstance" $ \o -> do
                contribution <- o .: "contribution"
                title <- o .: "hasTitle"
                instlanguage <- o .: "language"
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
                langcode <- o .: "code"
                return Language{..}

instance FromJSON Role where
        parseJSON = withObject "role" $ \o -> do
                roleid <- o .: "@id"
                return Role{..}

instance FromJSON Subject where
        parseJSON = withObject "subject" $ \o -> do
                subjcode <- o .: "code"
                prefLabel <- o .: "prefLabel"
                subjlanguage <- o .: "language"
                return Subject{..}

instance FromJSON Agent where
        parseJSON = genericParseJSON swepubOptions

instance FromJSON Title
instance FromJSON Source
instance FromJSON Publication
instance FromJSON Summary

instance FromJSON Affiliation where
        parseJSON = withObject "affiliation" $ \o -> do
                affname <- o .: "name"
                afflanguage <- o .: "language"
                affidentifiedBy <- o .: "identifiedBy"
                affaffiliation <- o .:? "hasAffiliation"
                return Affiliation{..}

swepubOptions = defaultOptions {sumEncoding = defaultTaggedObject{tagFieldName="@type"}}

toCfResPubl :: SwepubRecord -> CerifRecord
toCfResPubl sr = CerifRecord {
        resPubl = CfResPubl {cfResPublId = (swepubId sr)}
        , resPublTitle = titles sr
        , resPublAbstr = abstrs sr
        , resPublKeyw = keyws sr
        , pers = persons sr
        , persName = persnames sr
        , persName_Pers = persnames_pers sr
        , persResPubl = pers_respubl sr
        , orgUnit = ous sr
        , orgUnitName = ounames sr
}

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
ous sr = nub $ map toCfOrgUnit (concat (map (fromMaybe []) affs))
        where
                affs = concat [mainaffs, subaffs]
                mainaffs = map affiliation (contribution $ instanceOf sr)
                subaffs = map affaffiliation (concat (map (fromMaybe []) mainaffs))

ounames :: SwepubRecord -> [CfOrgUnitName]
ounames sr = nub $ map toCfOrgUnitName (concat (map (fromMaybe []) affs))
        where
                affs = concat [mainaffs, subaffs]
                mainaffs = map affiliation (contribution $ instanceOf sr)
                subaffs = map affaffiliation (concat (map (fromMaybe []) mainaffs))

toCfResPublTitle :: SwepubRecord -> Title -> CfResPublTitle
toCfResPublTitle sr t =
        CfResPublTitle {cfResPublId = (swepubId sr), cfLangCode = l, cfTrans = "o", cfTitle = mt}
                where
                        l = langcode $ instlanguage (instanceOf sr) !! 0
                        mt = mainTitle t

toCfResPublAbstr :: SwepubRecord -> Summary -> CfResPublAbstr
toCfResPublAbstr sr s =
        CfResPublAbstr {cfResPublId = (swepubId sr), cfLangCode = l, cfTrans = "o", cfAbstr = sl}
                where
                        l = langcode $ instlanguage (instanceOf sr) !! 0
                        sl = label s

toCfResPublKeyw :: SwepubRecord -> Subject -> CfResPublKeyw
toCfResPublKeyw sr s =
        CfResPublKeyw {cfResPublId = (swepubId sr), cfLangCode = l, cfTrans = "o", cfKeyw = pl}
                where
                        l = langcode $ subjlanguage s
                        pl = prefLabel s

toCfPers :: Agent -> CfPers
toCfPers a = CfPers {cfPersId = (identifierValue $ (identifiedBy a) !! 0)}

toCfPersName :: Agent -> CfPersName
toCfPersName a = CfPersName {
        cfPersNameId = (identifierValue $ (identifiedBy a) !! 0) ++ "-N"
        , cfFamilyNames = familyName a
        , cfFirstNames = givenName a
}

toCfPersName_Pers :: Agent -> CfPersName_Pers
toCfPersName_Pers a = CfPersName_Pers {
        cfPersId = (identifierValue $ (identifiedBy a) !! 0)
        , cfPersNameId = (identifierValue $ (identifiedBy a) !! 0) ++ "-N"
        , cfClassId = "SwepubName"
        , cfClassSchemeId = "FGS_Swepub"
        , cfStartDate = "1900-01-01T00:00:00"
        , cfEndDate = "2099-12-31T00:00:00"
}

toCfPers_ResPubl :: SwepubRecord -> Agent -> Role -> CfPers_ResPubl
toCfPers_ResPubl sr a r = CfPers_ResPubl {
        cfPersId = (identifierValue $ (identifiedBy a) !! 0)
        , cfResPublId = swepubId sr
        , cfClassId = roleid r
        , cfClassSchemeId = "FGS_Swepub"
        , cfStartDate = "1900-01-01T00:00:00"
        , cfEndDate = "2099-12-31T00:00:00"
}

toCfOrgUnit :: Affiliation -> CfOrgUnit
toCfOrgUnit a = CfOrgUnit {cfOrgUnitId = (identifierValue $ (affidentifiedBy a) !! 0)}

toCfOrgUnitName :: Affiliation -> CfOrgUnitName
toCfOrgUnitName a = CfOrgUnitName {
        cfOrgUnitId = (identifierValue $ (affidentifiedBy a) !! 0)
        , cfLangCode = l
        , cfTrans = "o"
        , cfName = n
}
        where
                l = langcode $ afflanguage a
                n = affname a

parse :: [String] -> IO ()
parse ["-s"] = biblput False
parse _ = biblput True

biblput :: Bool -> IO ()
biblput c = do
        biblin <- L.getContents
        let biblo = eitherDecode biblin :: Either String SwepubRecord
        case biblo of
                Left err -> do
                        hPutStrLn stderr err
                        exitWith (ExitFailure 1)
                Right biblrec ->
                        if c then do
                                putStrLn (show $ toCfResPubl biblrec)
                        else do
                                putStrLn (show biblrec)

main :: IO ()
main = getArgs >>= parse 
