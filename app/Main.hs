{-# LANGUAGE OverloadedLabels, OverloadedStrings, QuasiQuotes, DuplicateRecordFields, DeriveGeneric, RecordWildCards #-}

module Main where

import           Cerif
import           CerifXML
import           Data.Aeson
import           Data.Either
import           GHC.Generics
import           Swepub
import           System.Environment
import           System.IO (hPutStrLn, stderr)
import           Text.XML
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Lazy.IO as TIO

parse :: [String] -> IO ()
parse ["-s"] = biblput "swepub"
parse ["-x"] = biblput "cerifxml"
parse _ = biblput "cerif"

biblput :: String -> IO ()
biblput c = do
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
        case c of
                "cerif" -> putStrLn (show $ map toCfResPubl biblrec)
                "cerifxml" -> TIO.putStrLn (renderText def (toCerifXML (map toCfResPubl biblrec)))
                "swepub" -> putStrLn (show biblrec)
                _ -> putStrLn (show $ map toCfResPubl biblrec)

main :: IO ()
main = getArgs >>= parse 
