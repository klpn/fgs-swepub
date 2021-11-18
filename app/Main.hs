{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, DeriveGeneric, RecordWildCards #-}

module Main where

import           Data.Aeson
import           GHC.Generics
import           System.IO (hPutStrLn, stderr)
import qualified Data.ByteString.Lazy as L

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
        , language :: [Language]
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
        code :: String
}
        deriving (Show, Generic)

data Summary = Summary {
        label :: String
}
        deriving (Show, Generic)

data Subject = Subject {
        code :: String
        , prefLabel :: String
        , language :: Language
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
                return Contribution{..}

instance FromJSON Agent where
        parseJSON = genericParseJSON swepubOptions

instance FromJSON Title
instance FromJSON Source
instance FromJSON Publication
instance FromJSON Language
instance FromJSON Summary
instance FromJSON Subject

instance FromJSON Affiliation where
        parseJSON = withObject "affiliation" $ \o -> do
                name <- o .: "name"
                affiliation <- o .:? "hasAffiliation"
                return Affiliation{..}

swepubOptions = defaultOptions {sumEncoding = defaultTaggedObject{tagFieldName="@type"}}

main :: IO ()
main = do
        biblin <- L.getContents
        let biblo = eitherDecode biblin :: Either String SwepubRecord
        case biblo of
                Left err -> do
                        hPutStrLn stderr err
                Right biblrec -> do
                        putStrLn (show biblrec)

