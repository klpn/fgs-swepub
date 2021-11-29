{-# LANGUAGE OverloadedLabels, OverloadedStrings, QuasiQuotes, DuplicateRecordFields, DeriveGeneric, RecordWildCards #-}

module Swepub where

import           Control.Lens
import           Data.Generics.Labels
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
