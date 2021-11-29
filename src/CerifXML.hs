{-# LANGUAGE OverloadedLabels, OverloadedStrings, QuasiQuotes, DuplicateRecordFields, DeriveGeneric, RecordWildCards #-}

module CerifXML where

import           Cerif
import           Control.Lens
import           Data.Generics.Labels
import           Text.Hamlet.XML
import           Text.XML
import           qualified Data.Map.Strict as M
import qualified Data.Text as T

cerifattrs :: M.Map Name T.Text
cerifattrs = M.fromList [
        ("xmlns", T.pack "urn:xmlns:org:eurocris:cerif-1.6-2")
        , ("xsi:schemaLocation", T.pack $ "urn:xmlns:org:eurocris:cerif-1.6-2 " ++
                "https://www.eurocris.org/Uploads/Web%20pages/CERIF-1.6/CERIF_1.6_2.xsd")
        , ("xmlns:xsi", T.pack "http://www.w3.org/2001/XMLSchema-instance")
        , ("date", T.pack "2021-11-28")
        , ("sourceDatabase", T.pack "FGS_FM")]

toCerifXML :: CerifRecord -> Document
toCerifXML cr = Document (Prologue [] Nothing []) cerifroot []
        where
                cerifroot = Element "CERIF" cerifattrs [xml|
<cfResPubl>
        <cfResPublId>#{publId}
        <cfResPublDate>#{publDate}
$forall crt <- crts
        ^{toCerifXMLResPublTitle crt}
$forall crab <- crabs
        ^{toCerifXMLResPublAbstr crab}
$forall crkeyw <- crkeyws
        ^{toCerifXMLResPublKeyw crkeyw}
|]
                publId = cr ^. #resPubl . #cfResPublId
                publDate = cr ^. #resPubl . #cfResPublDate
                crts = cr ^. #resPublTitle
                crabs = cr ^. #resPublAbstr
                crkeyws = cr ^. #resPublKeyw

toCerifXMLResPublTitle :: CfResPublTitle -> [Node] 
toCerifXMLResPublTitle crt =  [xml|
<cfResPublTitle>
        <cfResPublId>#{publId}
        <cfTitle cfLangCode=#{langCode} cfTrans=#{trans}>#{title}
|]
        where
                publId = crt ^. #cfResPublId
                title = crt ^. #cfTitle
                langCode = crt ^. #cfLangCode
                trans = crt ^. #cfTrans

toCerifXMLResPublAbstr :: CfResPublAbstr -> [Node] 
toCerifXMLResPublAbstr crab =  [xml|
<cfResPublAbstr>
        <cfResPublId>#{publId}
        <cfAbstr cfLangCode=#{langCode} cfTrans=#{trans}>#{title}
|]
        where
                publId = crab ^. #cfResPublId
                title = crab ^. #cfAbstr
                langCode = crab ^. #cfLangCode
                trans = crab ^. #cfTrans

toCerifXMLResPublKeyw :: CfResPublKeyw -> [Node] 
toCerifXMLResPublKeyw crkeyw =  [xml|
<cfResPublKeyw>
        <cfResPublId>#{publId}
        <cfKeyw cfLangCode=#{langCode} cfTrans=#{trans}>#{title}
|]
        where
                publId = crkeyw ^. #cfResPublId
                title = crkeyw ^. #cfKeyw
                langCode = crkeyw ^. #cfLangCode
                trans = crkeyw ^. #cfTrans
