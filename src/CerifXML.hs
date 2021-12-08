{-# LANGUAGE OverloadedLabels, OverloadedStrings, QuasiQuotes, DuplicateRecordFields, DeriveGeneric, RecordWildCards #-}

module CerifXML where

import           Cerif
import           Conduit
import           Control.Lens hiding (matching)
import           Data.Generics.Labels
import           Data.List
import           Text.Hamlet.XML
import           Text.XML
import           Text.XML.Stream.Parse
import           qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.XML.Types as X

cerifns :: T.Text
cerifns = "urn:xmlns:org:eurocris:cerif-1.6-2"

cerifnsName :: String -> Name
cerifnsName s = Name (T.pack s) (Just cerifns) Nothing

matchCNN :: String -> NameMatcher Name
matchCNN = matching . (==) . cerifnsName

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


parseCerifRecord :: MonadThrow m => ConduitT X.Event o m (Maybe CerifRecord)
parseCerifRecord = tagIgnoreAttrs (matchCNN "CERIF") $ do
        resPubl <- many parseCfResPubl
        resPublTitle <- many parseCfResPublTitle
        resPublAbstr <- many parseCfResPublAbstr
        resPublKeyw <- many parseCfResPublKeyw
        pers <- many parseCfPers
        persName <- many parseCfPersName
        persName_Pers <- many parseCfPersName_Pers
        pers_ResPubl <- many parseCfPers_ResPubl
        orgUnit <- many parseCfOrgUnit
        orgUnitName <- many parseCfOrgUnitName
        return
                $ CerifRecord
                        resPubl
                        resPublTitle
                        resPublAbstr
                        resPublKeyw
                        pers
                        persName
                        persName_Pers
                        pers_ResPubl
                        orgUnit
                        orgUnitName

parseCfResPubl :: MonadThrow m => ConduitT X.Event o m (Maybe CfResPubl)
parseCfResPubl = tagNoAttr (matchCNN "cfResPubl") $ do
        publId <- force "publId missing" $ tagNoAttr (matchCNN "cfResPublId") content
        publDate <- force "publDate missing" $ tagNoAttr (matchCNN "cfResPublDate") content
        return
                $ CfResPubl
                        publId
                        publDate

parseCfResPublTitle :: MonadThrow m => ConduitT X.Event o m (Maybe CfResPublTitle)
parseCfResPublTitle = tagNoAttr (matchCNN "cfResPublTitle") $ do
        publId <- force "publId missing" $ tagNoAttr (matchCNN "cfResPublId") content
        force "title missing" $ tag' (matchCNN "cfTitle") parseTitleAttr $ \(langCode, trans) -> do
                title <- content
                return
                        $ CfResPublTitle
                                publId
                                langCode 
                                trans
                                title
                where
                        parseTitleAttr = (,) <$> requireAttr "cfLangCode" <*> requireAttr "cfTrans" <* ignoreAttrs

parseCfResPublAbstr :: MonadThrow m => ConduitT X.Event o m (Maybe CfResPublAbstr)
parseCfResPublAbstr = tagNoAttr (matchCNN "cfResPublAbstr") $ do
        publId <- force "publId missing" $ tagNoAttr (matchCNN "cfResPublId") content
        force "abstract missing" $ tag' (matchCNN "cfAbstr") parseAbstrAttr $ \(langCode, trans) -> do
                abstr <- content
                return
                        $ CfResPublAbstr
                                publId
                                langCode 
                                trans
                                abstr
                where
                        parseAbstrAttr = (,) <$> requireAttr "cfLangCode" <*> requireAttr "cfTrans" <* ignoreAttrs

parseCfResPublKeyw :: MonadThrow m => ConduitT X.Event o m (Maybe CfResPublKeyw)
parseCfResPublKeyw = tagNoAttr (matchCNN "cfResPublKeyw") $ do
        publId <- force "publId missing" $ tagNoAttr (matchCNN "cfResPublId") content
        force "keywords missing" $ tag' (matchCNN "cfKeyw") parseKeywAttr $ \(langCode, trans) -> do
                keyw <- content
                return
                        $ CfResPublKeyw
                                publId
                                langCode 
                                trans
                                keyw
                where
                        parseKeywAttr = (,) <$> requireAttr "cfLangCode" <*> requireAttr "cfTrans" <* ignoreAttrs

parseCfPers :: MonadThrow m => ConduitT X.Event o m (Maybe CfPers)
parseCfPers = tagNoAttr (matchCNN "cfPers") $ do
        persId <- force "persId missing" $ tagNoAttr (matchCNN "cfPersId") content
        return
                $ CfPers
                        persId

parseCfPersName :: MonadThrow m => ConduitT X.Event o m (Maybe CfPersName)
parseCfPersName = tagNoAttr (matchCNN "cfPersName") $ do
        persNameId <- force "persNameId missing" $ tagNoAttr (matchCNN "cfPersNameId") content
        familyNames <- force "familyNames missing" $ tagNoAttr (matchCNN "cfFamilyNames") content
        firstNames <- force "firstNames missing" $ tagNoAttr (matchCNN "cfFirstNames") content
        return
                $ CfPersName
                        persNameId
                        familyNames
                        firstNames

parseCfPersName_Pers :: MonadThrow m => ConduitT X.Event o m (Maybe CfPersName_Pers)
parseCfPersName_Pers = tagNoAttr (matchCNN "cfPersName_Pers") $ do
        persNameId <- force "persNameId missing" $ tagNoAttr (matchCNN "cfPersNameId") content
        persId <- force "persId missing" $ tagNoAttr (matchCNN "cfPersId") content
        classId <- force "classId missing" $ tagNoAttr (matchCNN "cfClassId") content
        classSchemeId <- force "classSchemeId missing" $ tagNoAttr (matchCNN "cfClassSchemeId") content
        startDate <- force "startDate missing" $ tagNoAttr (matchCNN "cfStartDate") content
        endDate <- force "endDate missing" $ tagNoAttr (matchCNN "cfEndDate") content
        return
                $ CfPersName_Pers
                        persNameId
                        persId
                        classId
                        classSchemeId
                        startDate
                        endDate

parseCfPers_ResPubl :: MonadThrow m => ConduitT X.Event o m (Maybe CfPers_ResPubl)
parseCfPers_ResPubl = tagNoAttr (matchCNN "cfPers_ResPubl") $ do
        persId <- force "persId missing" $ tagNoAttr (matchCNN "cfPersId") content
        resPublId <- force "resPublId missing" $ tagNoAttr (matchCNN "cfResPublId") content
        classId <- force "classId missing" $ tagNoAttr (matchCNN "cfClassId") content
        classSchemeId <- force "classSchemeId missing" $ tagNoAttr (matchCNN "cfClassSchemeId") content
        startDate <- force "startDate missing" $ tagNoAttr (matchCNN "cfStartDate") content
        endDate <- force "endDate missing" $ tagNoAttr (matchCNN "cfEndDate") content
        return
                $ CfPers_ResPubl
                        persId
                        resPublId
                        classId
                        classSchemeId
                        startDate
                        endDate

parseCfOrgUnit :: MonadThrow m => ConduitT X.Event o m (Maybe CfOrgUnit)
parseCfOrgUnit = tagNoAttr (matchCNN "cfOrgUnit") $ do
        orgUnitId <- force "orgUnitId missing" $ tagNoAttr (matchCNN "cfOrgUnitId") content
        return
                $ CfOrgUnit
                        orgUnitId

parseCfOrgUnitName :: MonadThrow m => ConduitT X.Event o m (Maybe CfOrgUnitName)
parseCfOrgUnitName = tagNoAttr (matchCNN "cfOrgUnitName") $ do
        orgUnitId <- force "orgUnitId missing" $ tagNoAttr (matchCNN "cfOrgUnitId") content
        force "name missing" $ tag' (matchCNN "cfName") parseNameAttr $ \(langCode, trans) -> do
                name <- content
                return
                        $ CfOrgUnitName
                                orgUnitId
                                langCode 
                                trans
                                name
                where
                        parseNameAttr = (,) <$> requireAttr "cfLangCode" <*> requireAttr "cfTrans" <* ignoreAttrs
