{-# LANGUAGE ScopedTypeVariables #-}
import Text.XML.HXT.Core
import Data.Csv
import qualified Data.ByteString.Lazy as BSL
{-Extracting attributes from tree-}
mining :: IO ()
mining = do
     input <- readFile "prerows.xml"
     nm <- runX $ readString [withValidate no] input
         //> hasName "name"
         //> getText
     id <- runX $ readString [withValidate no] input
         //> hasName "id"
         //> getText
     nt <- runX $ readString [withValidate no] input
         //> hasName "nametype"
         //> getText
     ms <- runX $ readString [withValidate no] input
         //> hasName "mass"
         //> getText
     fl <- runX $ readString [withValidate no] input
         //> hasName "fall"
         //> getText
     yr <- runX $ readString [withValidate no] input
         //> hasName "year"
         //> getText
     la <- runX $ readString [withValidate no] input
         //> hasName "reclat"
         //> getText
     lo <- runX $ readString [withValidate no] input
         //> hasName "reclong"
         //> getText
     allData nm id nt ms fl yr la lo
     --print names

{-Creating data structure with all the atributes-}
allData :: [([Char], [Char], [Char], [Char], [Char], [Char], [Char], [Char])]
allData = zip ['A'..'Z'] ['A'..'Z'] ['A'..'Z'] ['A'..'Z'] ['A'..'Z'] ['A'..'Z'] ['A'..'Z'] ['A'..'Z']

{-Writing the csv file with the allData data structure-}
writeCVS = BSL.writeFile "finish.csv" $ encode allData

--read :: [symbol] -> [hi]
