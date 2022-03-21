{-# LANGUAGE OverloadedLabels, OverloadedStrings, QuasiQuotes, DuplicateRecordFields, DeriveGeneric, RecordWildCards #-}

module ModsXML where

import           Cerif
import           Conduit
import           Control.Lens hiding (matching)
import           Data.Generics.Labels
import           Data.List
import           Text.Hamlet.XML
import           Text.XML
import           Text.XML.Stream.Parse
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.XML.Types as X

modsns :: T.Text
modsns = "http://www.loc.gov/mods/v3"
