{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Text.Regex.TDFA

import Control.Exception (IOException)
import qualified Control.Exception as Exception
import qualified Data.Foldable as Foldable

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

import Data.Csv
  ( DefaultOrdered(headerOrder)
  , Header
  , ToField(toField)
  , ToNamedRecord(toNamedRecord)
  , (.=)
  )
import qualified Data.Csv as Cassava

import Data.Text (Text)
import qualified Data.Text.Encoding as Text

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Data.List.Split

import qualified Control.Monad as Monad

data Meteorite =
     Mt
    {
        meteoriteName :: [Char],
        meteoriteID :: [Char],
        meteoriteStatus :: [Char],
        meteoriteMass :: [Char],
        meteoriteFell :: [Char],
        meteoriteYear :: [Char],
        meteoriteLatitude :: [Char],
        meteoriteLongitude :: [Char]
    }

instance Show Meteorite where
     show (Mt nm id nt ms fl yr la lo) =
          show nm ++ "," ++ show id ++ "," ++ show nt ++ "," ++ show ms ++ ","
          ++ show fl ++ "," ++ show yr ++ "," ++ show la ++ "," ++ show lo

instance ToNamedRecord Meteorite where
    toNamedRecord Mt{..} = Cassava.namedRecord
        [ "Name" .= meteoriteName
        , "ID" .= meteoriteID
        , "Status" .= meteoriteStatus
        , "Mass" .= meteoriteMass
        , "Fell" .= meteoriteFell
        , "Year" .= meteoriteYear
        , "Latitude" .= meteoriteLatitude
        , "Longitude" .= meteoriteLongitude
        ]

itemHeader :: Header
itemHeader = Vector.fromList
            [
                "Name",
                "ID",
                "Status",
                "Mass",
                "Fell",
                "Year",
                "Latitude",
                "Longitude"
            ]

instance DefaultOrdered Meteorite where
    headerOrder _ = Cassava.header
            [
                "Name",
                "ID",
                "Status",
                "Mass",
                "Fell",
                "Year",
                "Latitude",
                "Longitude"
            ]

encodeMeteorites :: Vector Meteorite -> ByteString
encodeMeteorites =  Cassava.encodeDefaultOrderedByName . Foldable.toList

catchShowIO :: IO a -> IO (Either String a)
catchShowIO action = fmap Right action `Exception.catch` handleIOException
            where
                handleIOException :: IOException -> IO (Either String a)
                handleIOException = return . Left . show

encodeMeteoritesToFile :: FilePath -> Vector Meteorite -> IO (Either String ())
encodeMeteoritesToFile filePath = catchShowIO . ByteString.writeFile filePath .
                                                                encodeMeteorites

listToVector :: [[[Char]]] -> [Meteorite]
                -> Vector Meteorite
listToVector [] meteorites = Vector.fromList meteorites
listToVector (x:xs) [] =
     let meteorite :: Meteorite
         meteorite = Mt {
              meteoriteName = element x,
              meteoriteID = elementk x 1,
              meteoriteStatus = elementk x 2,
              meteoriteMass = elementk x 4,
              meteoriteFell = elementk x 5,
              meteoriteYear = elementk x 6,
              meteoriteLatitude = elementk x 7,
              meteoriteLongitude = elementk x 8
         }
     in listToVector xs [meteorite]
listToVector (x:xs) m =
     let meteorite :: Meteorite
         meteorite = Mt {
              meteoriteName = element x,
              meteoriteID = elementk x 1,
              meteoriteStatus = elementk x 2,
              meteoriteMass = elementk x 4,
              meteoriteFell = elementk x 5,
              meteoriteYear = elementk x 6,
              meteoriteLatitude = elementk x 7,
              meteoriteLongitude = elementk x 8
         }
     in listToVector xs (m ++ [meteorite])

element :: [a] -> a
element []    = error "list too short"
element (x:xs) = x

elementk :: [a] -> Int -> a
elementk [] _     = error "list too short"
elementk (x:xs) 0 = x
elementk (_:xs) k = elementk xs (k - 1)

toYear :: [Char] -> [Char]
toYear yr = take 4 yr

goodOrBad :: [Char] -> [Char]
goodOrBad "Valid" = "Good"
goodOrBad "Relict" = "Bad"

extraction :: [[Char]] -> [Char] -> ([Char],[Char])
extraction attr meteorite =
     let isin :: Bool
         isin = (meteorite =~ (element attr))
         in
              if isin == True
                    then if (element attr) == "<nametype>"
                              then let split :: [[Char]]
                                       split = splitOn (elementk attr 1) (elementk (splitOn (element attr) meteorite) 1)
                                       in (goodOrBad (element split), elementk split 1)
                         else if (element attr) == "<year>"
                                   then let split :: [[Char]]
                                            split = splitOn (elementk attr 1) (elementk (splitOn (element attr) meteorite) 1)
                                            in (toYear (element split), elementk split 1)
                         else let split :: [[Char]]
                                  split = splitOn (elementk attr 1) (elementk (splitOn (element attr) meteorite) 1)
                                  in (element split, elementk split 1)
               else ("",meteorite)

extractAttributes :: [[Char]] -> [Char] -> [[Char]] -> [[Char]]
extractAttributes [] _ attributes = attributes
extractAttributes (x:xs) meteorite [] =
     let attr :: [[Char]]
         attr = splitOn ":" x
         in let split :: ([Char],[Char])
                split = extraction attr meteorite
                in extractAttributes xs (snd split) [fst split]
extractAttributes (x:xs) meteorite attributes =
     let attr :: [[Char]]
         attr = splitOn ":" x
         in let split :: ([Char],[Char])
                split = extraction attr meteorite
                in extractAttributes xs (snd split) (attributes ++ [fst split])

listOfMeteorites :: [[Char]] -> [[Char]] -> [[[Char]]] -> [[[Char]]]
listOfMeteorites [] attributes meteorite = meteorite
listOfMeteorites (x:xs) attributes [] =
     let meteorite :: [[Char]]
         meteorite = extractAttributes attributes x []
         in listOfMeteorites xs attributes [meteorite]
listOfMeteorites (x:xs) attributes meteorite =
     let met :: [[Char]]
         met = extractAttributes attributes x []
         in listOfMeteorites xs attributes (meteorite ++ [met])

{-Extracting attributes from tree-}
mining :: IO ()
mining = do
     input <- readFile "C:/Users/danie/Documents/Daniel/Universidad/4toSemestre/LenguajesFormalesyAutomatas/Lab2/prerows.xml"
     let rows :: [[Char]]
         rows = splitOn "</row>" input
     let attributes :: [[Char]]
         attributes = ["<name>:</name>","<id>:</id>","<nametype>:</nametype>",
                      "<recclass>:</recclass>","<mass>:</mass>","<fall>:</fall>"
                      ,"<year>:</year>","<reclat>:</reclat>",
                      "<reclong>:</reclong>"]
     let met :: [[[Char]]]
         met = listOfMeteorites rows attributes []
     let meteorites :: Vector Meteorite
         meteorites = listToVector met []
     print meteorites
     -- Monad.void (encodeMeteoritesToFile "meteoritos.csv" meteorites)
