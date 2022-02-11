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
}
        deriving (Show)

instance FromJSON SlupubRecord

instance FromJSON SluPublication where
        parseJSON = withObject "slupublication" $ \o -> do
                publId <- o .: "publ_id"
                publYear <- o .: "publ_year"
                title <- o .: "cftitle"
                return SluPublication{..}
