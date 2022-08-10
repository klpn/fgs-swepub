{-# LANGUAGE OverloadedLabels, OverloadedStrings, QuasiQuotes, DuplicateRecordFields, DeriveGeneric, RecordWildCards #-}

module Main where

import           Cerif
import           CerifXML
import           Conduit
import           Data.Aeson
import           Data.Either
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO (hPutStrLn, stderr)
import           Text.XML
import           Text.XML.Cursor ((&/), ($/), (>=>))
import qualified Slupub as SL
import qualified Swepub as SW
import qualified ModsXML as MX
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8 
import qualified Data.Text.Lazy.IO as TIO
import qualified Text.XML.Cursor as C
import qualified Text.XML.Stream.Parse as XP

data Flag = FromFormat String | ToFormat String
        deriving Show

usage :: String -> IO ()
usage err = do
        hPutStrLn stderr $ usageInfo (err ++ "\nusage: fgs-swepub options") options
        exitWith (ExitFailure 1)

options :: [OptDescr Flag]
options = [
        Option ['f'] ["from"] (ReqArg FromFormat "INFORMAT") "Input format"
        , Option ['t'] ["to"] (ReqArg ToFormat "OUTFORMAT") "Output format"]

biblput :: String -> String -> IO ()
biblput "slupubjson" t = do
        biblin <- L.getContents
        let biblo = eitherDecode biblin :: Either String SL.SlupubRecord
        case biblo of
                Left err -> hPutStrLn stderr err
                Right biblrec ->
                        cerifout [SL.toCfResPubl biblrec] t
biblput "swepubjson" t = do
        biblinRaw <- L.getContents
        let biblin = init $ L.split 10 biblinRaw
        let biblo = (\b -> eitherDecode b :: Either String SW.SwepubRecord) <$> biblin
        let biblerr = lefts biblo
        let biblrec = rights biblo
        if (length biblerr) > 0 
                then do
                        hPutStrLn stderr $ show biblerr
                else do
                        return ()
        cerifout (SW.toCfResPubl <$> biblrec) t
biblput "cerifxml" t = do
        cerifinRaw <- L.getContents
        let cerifin =  parseLBS_ def cerifinRaw
        let cerif = parseCerifRecord (C.fromDocument cerifin)
        cerifout [cerif] t
biblput "modsxml" t = do
        modsinRaw <- L.getContents
        let modsin = parseLBS_ def modsinRaw
        let mods = MX.parseModsRecord (C.fromDocument modsin)
        cerifout [MX.toCfResPubl mods] t
biblput _ _ = usage "unrecognized format"

cerifout :: [CerifRecord] -> String -> IO ()
cerifout crs t = do
        case t of
                "cerifnat" -> putStrLn (show crs)
                "cerifxml" -> TIO.putStrLn (renderText def (toCerifXML crs))
                "modsxml" ->  TIO.putStrLn (renderText def (MX.toModsXML $ crs !! 0))
                _ -> usage "unrecognized format"

main :: IO ()
main = do 
        args <- getArgs
        let (flags, opts, errs) = getOpt Permute options args
        if (length errs) > 0 || (length flags) == 0
                then do
                        usage (unlines errs)
                else do
                        let fromf = last [f | FromFormat f <- flags]
                        let tof = last [t | ToFormat t <- flags]
                        biblput fromf tof
