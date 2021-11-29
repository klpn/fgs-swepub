{-# LANGUAGE OverloadedLabels, OverloadedStrings, QuasiQuotes, DuplicateRecordFields, DeriveGeneric, RecordWildCards #-}

module Main where

import           Cerif
import           CerifXML
import           Control.Lens
import           Data.Aeson
import           Data.Generics.Labels
import           Data.List
import           Data.Maybe
import           GHC.Generics
import           Swepub
import           System.Environment
import           System.Exit
import           System.IO (hPutStrLn, stderr)
import           Text.XML
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TIO

parse :: [String] -> IO ()
parse ["-s"] = biblput "swepub"
parse ["-x"] = biblput "cerifxml"
parse _ = biblput "cerif"

biblput :: String -> IO ()
biblput c = do
        biblin <- L.getContents
        let biblo = eitherDecode biblin :: Either String SwepubRecord
        case biblo of
                Left err -> do
                        hPutStrLn stderr err
                        exitWith (ExitFailure 1)
                Right biblrec ->
                        case c of
                                "cerif" -> putStrLn (show $ toCfResPubl biblrec)
                                "cerifxml" -> TIO.putStrLn (renderText def (toCerifXML (toCfResPubl biblrec)))
                                "swepub" -> putStrLn (show biblrec)
                                _ -> putStrLn (show $ toCfResPubl biblrec)

main :: IO ()
main = getArgs >>= parse 
