{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, DeriveGeneric, RecordWildCards #-}

module Main where

import           Data.Aeson
import           GHC.Generics
import           System.Environment
import           System.IO (hPutStrLn, stderr)
import qualified Data.ByteString.Lazy as L

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

data CfPers = CfPers {
        cfPersId :: String
}
        deriving (Show)

data CfOrgUnit = CfOrgUnit {
        cfOrgUnitId :: String
}
        deriving (Show)

data SwepubRecord = SwepubRecord {
        swepubId :: String
        , identifiedBy :: [Identifier]
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
        name :: String
        , affiliation :: Maybe [Affiliation]
}
        deriving (Show)

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
                return Contribution{..}

instance FromJSON Language where
        parseJSON = withObject "language" $ \o -> do
                langcode <- o .: "code"
                return Language{..}

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
                name <- o .: "name"
                affiliation <- o .:? "hasAffiliation"
                return Affiliation{..}

swepubOptions = defaultOptions {sumEncoding = defaultTaggedObject{tagFieldName="@type"}}

toCfResPubl :: SwepubRecord -> (CfResPubl, [CfResPublTitle])
toCfResPubl sr = (CfResPubl {cfResPublId = (swepubId sr)}, titles sr)

titles :: SwepubRecord -> [CfResPublTitle]
titles sr = map (\t -> toCfResPublTitle sr t) (title $ instanceOf sr)

toCfResPublTitle :: SwepubRecord -> Title -> CfResPublTitle
toCfResPublTitle sr t =
        CfResPublTitle {cfResPublId = (swepubId sr), cfLangCode = l, cfTrans = "o", cfTitle = mt}
                where
                        l = langcode $ instlanguage (instanceOf sr) !! 0
                        mt = mainTitle t

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
                Right biblrec ->
                        if c then do
                                putStrLn (show $ toCfResPubl biblrec)
                        else do
                                putStrLn (show biblrec)

main :: IO ()
main = getArgs >>= parse 
