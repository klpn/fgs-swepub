{-# LANGUAGE OverloadedLabels, OverloadedStrings, QuasiQuotes, DuplicateRecordFields, DeriveGeneric, RecordWildCards #-}

module Main where

import           Cerif
import           CerifXML
import           Data.Aeson
import           Data.Either
import           Swepub
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO (hPutStrLn, stderr)
import           Text.XML
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy.IO as TIO

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
biblput "swepubjson" t = do
        biblinRaw <- L.getContents
        let biblin = init $ L.split 10 biblinRaw
        let biblo = map (\b -> eitherDecode b :: Either String SwepubRecord) biblin
        let biblerr = lefts biblo
        let biblrec = rights biblo
        if (length biblerr) > 0 
                then do
                        hPutStrLn stderr $ show biblerr
                else do
                        return ()
        case t of
                "cerifnat" -> putStrLn (show $ map toCfResPubl biblrec)
                "cerifxml" -> TIO.putStrLn (renderText def (toCerifXML (map toCfResPubl biblrec)))
                "swepubnat" -> putStrLn (show biblrec)
                _ -> usage "unrecognized format"
biblput _ _ = usage "unrecognized format"

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
