{-# LANGUAGE OverloadedLabels, OverloadedStrings, QuasiQuotes, DuplicateRecordFields, DeriveGeneric, RecordWildCards #-}

module ModsXML where

import           Cerif
import           Conduit
import           Control.Applicative ((<|>))
import           Control.Lens
import           Data.Generics.Labels
import           Data.List
import           Data.Maybe
import           GHC.Generics
import           Text.Hamlet.XML
import           Text.XML
import           Text.XML.Cursor ((&/), ($/), ($//), (>=>))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.XML.Types as X
import qualified Text.XML.Cursor as C

modsns :: T.Text
modsns = "http://www.loc.gov/mods/v3"

modsnsName ::  String -> Name
modsnsName s = Name (T.pack s) (Just modsns) Nothing

data ModsRecord = ModsRecord {
        recordInfo :: ModsRecordInfo
        , genre :: [ModsGenre]
        , originInfo :: [ModsOriginInfo]
        , language :: [ModsLanguage]
        , titleInfo :: [ModsTitleInfo]
        , name :: [ModsName]
        , abstract :: [T.Text]
        , identifier :: [ModsIdentifier]
        , subject :: [ModsSubject]
}
        deriving (Show, Generic)

data ModsRecordInfo = ModsRecordInfo {
        recordContentSource :: T.Text
        , recordIdentifier :: T.Text
}
        deriving (Show, Generic)

data ModsGenre = ModsGenre {
        genreTerm :: T.Text
}
        deriving (Show, Generic)

data ModsOriginInfo = ModsOriginInfo {
        dateIssued :: Maybe Integer
        , publisher :: Maybe T.Text
}
        deriving (Show, Generic)

data ModsLanguage = ModsLanguage {
        languageTerm :: T.Text
}
        deriving (Show, Generic)

data ModsTitleInfo = ModsTitleInfo {
        title :: T.Text
}
        deriving (Show, Generic)

data ModsName = ModsName {
        nameType :: T.Text
        , languageTerm :: T.Text
        , namePart :: [ModsIdentifier]
        , nameRole :: [ModsRole]
        , nameIdentifier :: [ModsIdentifier]
}
        deriving (Show, Generic)

data ModsIdentifier = ModsIdentifier {
        identifierType :: T.Text
        , identifierValue :: T.Text
}
        deriving (Show, Generic)

data ModsSubject = ModsSubject {
        languageTerm :: T.Text
        , topic :: T.Text
}
        deriving (Show, Generic)

data ModsRole = ModsRole {
        roleTerm :: T.Text
}
        deriving (Show, Generic)

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

issdates :: [ModsOriginInfo] -> [Integer]
issdates moi = catMaybes (dateIssued <$> moi)

publclasses :: [ModsGenre] -> [T.Text]
publclasses mgi = catMaybes ((\g -> M.lookup (genreTerm g) cfclasses) <$> mgi)

toCfResPubl :: ModsRecord -> CerifRecord
toCfResPubl mr = CerifRecord {
        resPubl = [CfResPubl {cfResPublId = (recordIdentifier $ recordInfo mr)
                , cfResPublDate = T.pack $ show ((issdates $ originInfo mr) !! 0) ++ "-01-01"}]
        , resPublTitle = titles mr 
        , resPublAbstr = abstrs mr
        , resPublKeyw = keyws mr
        , resPubl_Class = rpclasses mr
        , pers = persons mr
        , persName = persnames mr
        , persName_Pers = persnames_pers mr 
        , pers_ResPubl = pers_respubl mr
        , orgUnit = ous mr
        , orgUnitName = ounames mr
}

titles :: ModsRecord -> [CfResPublTitle]
titles mr = (\t -> toCfResPublTitle mr t) <$> (titleInfo mr)

abstrs :: ModsRecord -> [CfResPublAbstr]
abstrs mr = (\a -> toCfResPublAbstr mr a) <$> (abstract mr)

rpclasses :: ModsRecord -> [CfResPubl_Class]
rpclasses mr = (\c -> toCfResPubl_Class mr c) <$> (publclasses $ genre mr)

keyws :: ModsRecord -> [CfResPublKeyw]
keyws mr = (\s -> toCfResPublKeyw mr s) <$> (subject mr)

persons :: ModsRecord -> [CfPers]
persons mr = toCfPers <$> [n | n <- (name mr), (nameType n) == "personal"]

persnames :: ModsRecord -> [CfPersName]
persnames mr = toCfPersName <$> [n | n <- (name mr), (nameType n) == "personal"]

persnames_pers :: ModsRecord -> [CfPersName_Pers]
persnames_pers mr = toCfPersName_Pers <$> [n | n <- (name mr), (nameType n) == "personal"]

pers_respubl :: ModsRecord -> [CfPers_ResPubl]
pers_respubl mr = concat $ (\n -> pers_respublName mr n) <$> [n | n <- (name mr), (nameType n) == "personal"]

pers_respublName :: ModsRecord -> ModsName -> [CfPers_ResPubl]
pers_respublName mr n = (\r -> toCfPers_ResPubl mr n r) <$> (nameRole n)

ous :: ModsRecord -> [CfOrgUnit]
ous mr = toCfOrgUnit <$> [n | n <- (name mr), (nameType n) == "corporate"]

ounames :: ModsRecord -> [CfOrgUnitName]
ounames mr = toCfOrgUnitName <$> [n | n <- (name mr), (nameType n) == "corporate"]

toCfResPublTitle :: ModsRecord -> ModsTitleInfo -> CfResPublTitle
toCfResPublTitle mr t =
        CfResPublTitle {cfResPublId = ri, cfLangCode = l, cfTrans = "o", cfTitle = title t}
                where
                        ri = recordIdentifier $ recordInfo mr
                        l = (language mr !! 0) ^. #languageTerm

toCfResPublAbstr :: ModsRecord -> T.Text -> CfResPublAbstr
toCfResPublAbstr mr a =
        CfResPublAbstr {cfResPublId = ri, cfLangCode = l, cfTrans = "o", cfAbstr = a}
                where
                        ri = recordIdentifier $ recordInfo mr
                        l = (language mr !! 0) ^. #languageTerm


toCfResPubl_Class :: ModsRecord -> T.Text -> CfResPubl_Class
toCfResPubl_Class mr c =
        CfResPubl_Class {
                        cfResPublId = recordIdentifier $ recordInfo mr
                        , cfClassId = c
                        , cfClassSchemeId = "759af938-34ae-11e1-b86c-0800200c9a66" 
                        , cfStartDate = "1900-01-01T00:00:00"
                        , cfEndDate = "2099-12-31T00:00:00"
                }

toCfResPublKeyw :: ModsRecord -> ModsSubject -> CfResPublKeyw
toCfResPublKeyw mr s =
        CfResPublKeyw {cfResPublId = ri, cfLangCode = l, cfTrans = "o", cfKeyw = t}
                where
                        ri = recordIdentifier $ recordInfo mr
                        l = s ^. #languageTerm
                        t = s ^. #topic

toCfPers :: ModsName -> CfPers
toCfPers n = CfPers {cfPersId = identifierValue $ (n ^. #nameIdentifier) !! 0}

toCfPersName :: ModsName -> CfPersName
toCfPersName n = CfPersName {
        cfPersNameId = T.concat [(identifierValue $ (n ^. #nameIdentifier) !! 0), "-N"]
        , cfFamilyNames = T.intercalate " "
                [identifierValue p | p <- (namePart n), (identifierType p) == "family"]
        , cfFirstNames = T.intercalate " "
                [identifierValue p | p <- (namePart n), (identifierType p) == "given"]
}

toCfPersName_Pers :: ModsName -> CfPersName_Pers
toCfPersName_Pers n = CfPersName_Pers {
        cfPersId = identifierValue $ (n ^. #nameIdentifier) !! 0
        , cfPersNameId = T.concat [(identifierValue $ (n ^. #nameIdentifier) !! 0), "-N"]
        , cfClassId = "ModsName"
        , cfClassSchemeId = "759af938-34ae-11e1-b86c-0800200c9a66"
        , cfStartDate = "1900-01-01T00:00:00"
        , cfEndDate = "2099-12-31T00:00:00"
}

toCfPers_ResPubl :: ModsRecord -> ModsName -> ModsRole -> CfPers_ResPubl
toCfPers_ResPubl mr n r = CfPers_ResPubl {
        cfPersId = identifierValue $ (n ^. #nameIdentifier) !! 0
        , cfResPublId = recordIdentifier $ recordInfo mr
        , cfClassId = roleTerm r
        , cfClassSchemeId = "759af938-34ae-11e1-b86c-0800200c9a66"
        , cfStartDate = "1900-01-01T00:00:00"
        , cfEndDate = "2099-12-31T00:00:00"
}

toCfOrgUnit :: ModsName -> CfOrgUnit
toCfOrgUnit n = CfOrgUnit {cfOrgUnitId = identifierValue $ (n ^. #namePart) !! 0}

toCfOrgUnitName :: ModsName -> CfOrgUnitName
toCfOrgUnitName n = CfOrgUnitName {
        cfOrgUnitId = identifierValue $ (n ^. #namePart) !! 0
        , cfLangCode = n ^. #languageTerm
        , cfTrans = "o"
        , cfName = identifierValue $ (n ^. #namePart) !! 0
}

parseModsRecord :: C.Cursor -> ModsRecord
parseModsRecord c = do
        let recordInfo = c $/ C.element (modsnsName "recordInfo") >=> parseRecordInfo
        let genre = c $/ C.element (modsnsName "genre") &/ C.content
        let originInfo = c $/ C.element (modsnsName "originInfo") >=> parseOriginInfo
        let language = c $/ C.element (modsnsName "language") >=> parseLanguage
        let titleInfo = c $/ C.element (modsnsName "titleInfo") >=> parseTitleInfo
        let name = c $/ C.element (modsnsName "name") >=> parseName
        let abstract = c $/ C.element (modsnsName "abstract") &/ C.content
        let identifier = c $/ C.element (modsnsName "identifier") >=> parseIdentifier
        let subject = c $/ C.element (modsnsName "subject") >=> parseSubject
        ModsRecord
                (recordInfo !! 0)
                (ModsGenre <$> genre)
                originInfo
                language
                titleInfo
                name
                abstract
                identifier
                subject

parseRecordInfo :: C.Cursor -> [ModsRecordInfo]
parseRecordInfo c = do
        let recordContentSource = c $/ C.element (modsnsName "recordContentSource") &/ C.content
        let recordIdentifier = c $/ C.element (modsnsName "recordIdentifier") &/ C.content
        [ModsRecordInfo (recordContentSource !! 0) (recordIdentifier !! 0)]

parseOriginInfo :: C.Cursor -> [ModsOriginInfo]
parseOriginInfo c = do
        let dateIssued = c $/ C.element (modsnsName "dateIssued") &/ C.content
        let publisher = c $/ C.element (modsnsName "publisher") &/ C.content
        [ModsOriginInfo (read <$> T.unpack <$> (dateIssued ^? element 0)) (publisher ^? element 0)]

parseLanguage :: C.Cursor -> [ModsLanguage]
parseLanguage c = do
        let languageTerm = c $/ C.element (modsnsName "languageTerm") &/ C.content
        [ModsLanguage (languageTerm !! 0)]

parseTitleInfo :: C.Cursor -> [ModsTitleInfo]
parseTitleInfo c = do
        let title = c $/ C.element (modsnsName "title") &/ C.content
        [ModsTitleInfo (title !! 0)]

parseName :: C.Cursor -> [ModsName]
parseName c = do
        let nameType = C.attribute "type" c
        let namePart = c $/ C.element (modsnsName "namePart") >=> parseNamePart
        let nameRole = c $/ C.element (modsnsName "role") >=> parseRole
        let nameIdentifier = c $/ C.element (modsnsName "nameIdentifier") >=> parseIdentifier
        [ModsName (nameType !! 0)  "swe" namePart nameRole nameIdentifier]

parseNamePart :: C.Cursor -> [ModsIdentifier]
parseNamePart c = do
        let namePartType = C.attribute "type" c
        let namePartValue = c $// C.content
        [ModsIdentifier (fromMaybe "unspec" $ namePartType ^? element 0) (namePartValue !! 0)]

parseRole :: C.Cursor -> [ModsRole]
parseRole c = do
        let roleTerm = c $/ C.element (modsnsName "roleTerm") &/ C.content
        [ModsRole (roleTerm !! 0)]

parseSubject :: C.Cursor -> [ModsSubject]
parseSubject c = do
        let languageTerm = C.attribute "lang" c
        let topic = c $/ C.element (modsnsName "topic") &/ C.content
        [ModsSubject (languageTerm !! 0) (topic !! 0)]

parseIdentifier :: C.Cursor -> [ModsIdentifier]
parseIdentifier c = do
        let identifierType = C.attribute "type" c
        let identifierValue = c $// C.content
        [ModsIdentifier (identifierType !! 0) (identifierValue !! 0)]
