{-# LANGUAGE OverloadedLabels, OverloadedStrings, QuasiQuotes, DuplicateRecordFields, DeriveGeneric, RecordWildCards #-}

module CerifXML where

import           Cerif
import           Conduit
import           Control.Lens hiding (matching)
import           Data.Generics.Labels
import           Data.List
import           Text.Hamlet.XML
import           Text.XML
import           Text.XML.Cursor ((&/), ($/), ($//), (>=>))
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.XML.Types as X
import qualified Text.XML.Cursor as C

cerifns :: T.Text
cerifns = "urn:xmlns:org:eurocris:cerif-1.6-2"

cerifnsName :: String -> Name
cerifnsName s = Name (T.pack s) (Just cerifns) Nothing

cerifattrs :: M.Map Name T.Text
cerifattrs = M.fromList [
        ("xmlns", cerifns)
        , ("xsi:schemaLocation", T.pack $ "urn:xmlns:org:eurocris:cerif-1.6-2 " ++
                "https://www.eurocris.org/Uploads/Web%20pages/CERIF-1.6/CERIF_1.6_2.xsd")
        , ("xmlns:xsi", T.pack "http://www.w3.org/2001/XMLSchema-instance")
        , ("date", T.pack "2021-11-28")
        , ("sourceDatabase", T.pack "FGS_FM")]

toCerifXML :: [CerifRecord] -> Document
toCerifXML crs = Document (Prologue [] Nothing []) cerifroot []
        where
                cerifroot = Element "CERIF" cerifattrs [xml|
$forall crpu <- crpus
        ^{toCerifXMLResPubl crpu}
$forall crt <- crts
        ^{toCerifXMLResPublTitle crt}
$forall crab <- crabs
        ^{toCerifXMLResPublAbstr crab}
$forall crkeyw <- crkeyws
        ^{toCerifXMLResPublKeyw crkeyw}
$forall crp <- crps
        ^{toCerifXMLPers crp}
$forall crpn <- crpns
        ^{toCerifXMLPersName crpn}
$forall crpnp <- crpnps
        ^{toCerifXMLPersName_Pers crpnp}
$forall crprp <- crprps
        ^{toCerifXMLPers_ResPubl crprp}
$forall crou <- crous
        ^{toCerifXMLOrgUnit crou}
$forall croun <- crouns
        ^{toCerifXMLOrgUnitName croun}
|]
                crpus = nub $ concat $ (\cr -> cr ^. #resPubl) <$> crs
                crts = nub $ concat $ (\cr -> cr ^. #resPublTitle) <$> crs
                crabs = nub $ concat $ (\cr -> cr ^. #resPublAbstr) <$> crs
                crkeyws = nub $ concat $ (\cr -> cr ^. #resPublKeyw) <$> crs
                crps = nub $ concat $ (\cr -> cr ^. #pers) <$> crs
                crpns = nub $ concat $ (\cr -> cr ^. #persName) <$> crs
                crpnps = nub $ concat $ (\cr -> cr ^. #persName_Pers) <$> crs
                crprps = nub $ concat $ (\cr -> cr ^. #pers_ResPubl) <$> crs
                crous = nub $ concat $ (\cr -> cr ^. #orgUnit) <$> crs
                crouns = nub $ concat $ (\cr -> cr ^. #orgUnitName) <$> crs

toCerifXMLResPubl :: CfResPubl -> [Node] 
toCerifXMLResPubl crpu = [xml|
<cfResPubl>
        <cfResPublId>#{publId}
        <cfResPublDate>#{publDate}
|]
        where
                publId = crpu ^. #cfResPublId
                publDate = crpu ^. #cfResPublDate

toCerifXMLResPublTitle :: CfResPublTitle -> [Node] 
toCerifXMLResPublTitle crt = [xml|
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
toCerifXMLResPublAbstr crab = [xml|
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
toCerifXMLResPublKeyw crkeyw = [xml|
<cfResPublKeyw>
        <cfResPublId>#{publId}
        <cfKeyw cfLangCode=#{langCode} cfTrans=#{trans}>#{title}
|]
        where
                publId = crkeyw ^. #cfResPublId
                title = crkeyw ^. #cfKeyw
                langCode = crkeyw ^. #cfLangCode
                trans = crkeyw ^. #cfTrans

toCerifXMLPers :: CfPers -> [Node] 
toCerifXMLPers crp = [xml|
<cfPers>
        <cfPersId>#{persId}
|]
        where
                persId = crp ^. #cfPersId

toCerifXMLPersName :: CfPersName -> [Node] 
toCerifXMLPersName crpn = [xml|
<cfPersName>
        <cfPersNameId>#{persNameId}
        <cfFamilyNames>#{familyNames}
        <cfFirstNames>#{firstNames}
|]
        where
                persNameId = crpn ^. #cfPersNameId
                familyNames = crpn ^. #cfFamilyNames
                firstNames = crpn ^. #cfFirstNames

toCerifXMLPersName_Pers :: CfPersName_Pers -> [Node] 
toCerifXMLPersName_Pers crpnp = [xml|
<cfPersName_Pers>
        <cfPersNameId>#{persNameId}
        <cfPersId>#{persId}
        <cfClassId>#{classId}
        <cfClassSchemeId>#{classSchemeId}
        <cfStartDate>#{startDate}
        <cfEndDate>#{endDate}
|]
        where
                persNameId = crpnp ^. #cfPersNameId
                persId = crpnp ^. #cfPersId
                classId = crpnp ^. #cfClassId
                classSchemeId = crpnp ^. #cfClassSchemeId
                startDate = crpnp ^. #cfStartDate
                endDate = crpnp ^. #cfEndDate

toCerifXMLPers_ResPubl :: CfPers_ResPubl -> [Node] 
toCerifXMLPers_ResPubl crprp = [xml|
<cfPers_ResPubl>
        <cfPersId>#{persId}
        <cfResPublId>#{resPublId}
        <cfClassId>#{classId}
        <cfClassSchemeId>#{classSchemeId}
        <cfStartDate>#{startDate}
        <cfEndDate>#{endDate}
|]
        where
                persId = crprp ^. #cfPersId
                resPublId = crprp ^. #cfResPublId
                classId = crprp ^. #cfClassId
                classSchemeId = crprp ^. #cfClassSchemeId
                startDate = crprp ^. #cfStartDate
                endDate = crprp ^. #cfEndDate

toCerifXMLOrgUnit :: CfOrgUnit -> [Node] 
toCerifXMLOrgUnit crou = [xml|
<cfOrgUnit>
        <cfOrgUnitId>#{orgUnitId}
|]
        where
                orgUnitId = crou ^. #cfOrgUnitId

toCerifXMLOrgUnitName :: CfOrgUnitName -> [Node] 
toCerifXMLOrgUnitName oun = [xml|
<cfOrgUnitName>
        <cfOrgUnitId>#{orgUnitId}
        <cfName cfLangCode=#{langCode} cfTrans=#{trans}>#{name}
|]
        where
                orgUnitId = oun ^. #cfOrgUnitId
                name = oun ^. #cfName
                langCode = oun ^. #cfLangCode
                trans = oun ^. #cfTrans

parseCerifRecord :: C.Cursor -> CerifRecord
parseCerifRecord c = do
        let resPubl =  c $/ C.element (cerifnsName "cfResPubl") >=> parseCfResPubl
        let resPublTitle = c $/ C.element (cerifnsName "cfResPublTitle") >=> parseCfResPublTitle
        let resPublAbstr = c $/ C.element (cerifnsName "cfResPublAbstr") >=> parseCfResPublAbstr
        let resPublKeyw = c $/ C.element (cerifnsName "cfResPublKeyw") >=> parseCfResPublKeyw
        let resPubl_Class = c $/ C.element (cerifnsName "cfResPublClass") >=> parseCfResPubl_Class
        let pers = c $/ C.element (cerifnsName "cfPers") >=> parseCfPers
        let persName = c $/ C.element (cerifnsName "cfPersName") >=> parseCfPersName
        let persName_Pers = c $/ C.element (cerifnsName "cfPersName_Pers") >=> parseCfPersName_Pers
        let pers_ResPubl = c $/ C.element (cerifnsName "cfPers_ResPubl") >=> parseCfPers_ResPubl
        let orgUnit = c $/ C.element (cerifnsName "cfOrgUnit") >=> parseCfOrgUnit
        let orgUnitName = c $/ C.element (cerifnsName "cfOrgUnitName") >=> parseCfOrgUnitName
        CerifRecord
                resPubl
                resPublTitle
                resPublAbstr
                resPublKeyw
                resPubl_Class
                pers
                persName
                persName_Pers
                pers_ResPubl
                orgUnit
                orgUnitName

parseCfResPubl :: C.Cursor -> [CfResPubl]
parseCfResPubl c = do
        let publId = c $/ C.element (cerifnsName "cfResPublId") &/ C.content
        let publDate = c $/ C.element (cerifnsName "cfResPublDate") &/ C.content
        [CfResPubl (publId !! 0) (publDate !! 0)]

parseCfResPublTitle :: C.Cursor -> [CfResPublTitle]
parseCfResPublTitle c = do
        let publId = c $/ C.element (cerifnsName "cfResPublId") &/ C.content
        let t = c $// C.element (cerifnsName "cfTitle")
        let langCode = C.attribute "cfLangCode" (t !! 0)
        let trans = C.attribute "cfTrans" (t !! 0)
        let t0 = C.child $ t !! 0
        let title = C.content $ t0 !! 0
        [CfResPublTitle (publId !! 0) (langCode !! 0) (trans !! 0) (title !! 0)]

parseCfResPublAbstr :: C.Cursor -> [CfResPublAbstr]
parseCfResPublAbstr c = do
        let publId = c $/ C.element (cerifnsName "cfResPublId") &/ C.content
        let a = c $// C.element (cerifnsName "cfAbstr")
        let langCode = C.attribute "cfLangCode" (a !! 0)
        let trans = C.attribute "cfTrans" (a !! 0)
        let a0 = C.child $ a !! 0
        let abstr = C.content $ a0 !! 0
        [CfResPublAbstr (publId !! 0) (langCode !! 0) (trans !! 0) (abstr !! 0)]

parseCfResPublKeyw :: C.Cursor -> [CfResPublKeyw]
parseCfResPublKeyw c = do
        let publId = c $/ C.element (cerifnsName "cfResPublId") &/ C.content
        let k = c $// C.element (cerifnsName "cfKeyw")
        let langCode = C.attribute "cfLangCode" (k !! 0)
        let trans = C.attribute "cfTrans" (k !! 0)
        let k0 = C.child $ k !! 0
        let keyw = C.content $ k0 !! 0
        [CfResPublKeyw (publId !! 0) (langCode !! 0) (trans !! 0) (keyw !! 0)]

parseCfResPubl_Class :: C.Cursor -> [CfResPubl_Class]
parseCfResPubl_Class c = do
        let publId = c $/ C.element (cerifnsName "cfResPublId") &/ C.content
        let classId = c $/ C.element (cerifnsName "cfClassId") &/ C.content
        let classSchemeId = c $/ C.element (cerifnsName "cfClassSchemeId") &/ C.content
        let startDate = c $/ C.element (cerifnsName "cfStartDate") &/ C.content
        let endDate = c $/ C.element (cerifnsName "cfEndDate") &/ C.content
        [CfResPubl_Class (publId !! 0) (classId !! 0) (classSchemeId !! 0) (startDate !! 0) (endDate !! 0)]

parseCfPers :: C.Cursor -> [CfPers]
parseCfPers c = do
        let persId = c $/ C.element (cerifnsName "cfPersId") &/ C.content
        [CfPers (persId !! 0)]

parseCfPersName :: C.Cursor -> [CfPersName]
parseCfPersName c = do
        let persNameId = c $/ C.element (cerifnsName "cfPersNameId") &/ C.content
        let familyNames = c $/ C.element (cerifnsName "cfFamilyNames") &/ C.content
        let firstNames = c $/ C.element (cerifnsName "cfFirstNames") &/ C.content
        [CfPersName (persNameId !! 0) (familyNames !! 0) (firstNames !!0)]

parseCfPersName_Pers :: C.Cursor -> [CfPersName_Pers]
parseCfPersName_Pers c = do
        let persNameId = c $/ C.element (cerifnsName "cfPersNameId") &/ C.content
        let persId = c $/ C.element (cerifnsName "cfPersId") &/ C.content
        let classId = c $/ C.element (cerifnsName "cfClassId") &/ C.content
        let classSchemeId = c $/ C.element (cerifnsName "cfClassSchemeId") &/ C.content
        let startDate = c $/ C.element (cerifnsName "cfStartDate") &/ C.content
        let endDate = c $/ C.element (cerifnsName "cfEndDate") &/ C.content
        [CfPersName_Pers (persNameId !! 0) (persId !! 0) (classId !! 0) (classSchemeId !! 0) (startDate !! 0) (endDate !! 0)]

parseCfPers_ResPubl :: C.Cursor -> [CfPers_ResPubl]
parseCfPers_ResPubl c = do
        let persId = c $/ C.element (cerifnsName "cfPersId") &/ C.content
        let publId = c $/ C.element (cerifnsName "cfResPublId") &/ C.content
        let classId = c $/ C.element (cerifnsName "cfClassId") &/ C.content
        let classSchemeId = c $/ C.element (cerifnsName "cfClassSchemeId") &/ C.content
        let startDate = c $/ C.element (cerifnsName "cfStartDate") &/ C.content
        let endDate = c $/ C.element (cerifnsName "cfEndDate") &/ C.content
        [CfPers_ResPubl (persId !! 0) (publId !! 0) (classId !! 0) (classSchemeId !! 0) (startDate !! 0) (endDate !! 0)]

parseCfOrgUnit :: C.Cursor -> [CfOrgUnit]
parseCfOrgUnit c = do
        let orgUnitId = c $/ C.element (cerifnsName "cfOrgUnitId") &/ C.content
        [CfOrgUnit (orgUnitId !! 0)]

parseCfOrgUnitName :: C.Cursor -> [CfOrgUnitName]
parseCfOrgUnitName c = do
        let orgUnitId = c $/ C.element (cerifnsName "cfOrgUnitId") &/ C.content
        let n = c $// C.element (cerifnsName "cfName")
        let langCode = C.attribute "cfLangCode" (n !! 0)
        let trans = C.attribute "cfTrans" (n !! 0)
        let n0 = C.child $ n !! 0
        let name = C.content $ n0 !! 0
        [CfOrgUnitName (orgUnitId !! 0) (langCode !! 0) (trans !! 0) (name !! 0)]
