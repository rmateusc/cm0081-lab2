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

listToVector :: [[Char]] -> [[Char]] -> [[Char]] -> [[Char]] -> [[Char]]
                -> [[Char]] -> [[Char]] -> [[Char]] -> [Meteorite]
                -> Vector Meteorite
listToVector [] _ _ _ _ _ _ _ meteorites = Vector.fromList meteorites
listToVector (nm:ss) (id:ts) (nt:us) (ms:vs) (fl:ws) (yr:xs) (la:ys) (lo:zs) [] =
     let meteorite :: Meteorite
         meteorite = Mt {
              meteoriteName = nm,
              meteoriteID = id,
              meteoriteStatus = nt,
              meteoriteMass = ms,
              meteoriteFell = fl,
              meteoriteYear = yr,
              meteoriteLatitude = la,
              meteoriteLongitude = lo
         }
     in listToVector ss ts us vs ws xs ys zs [meteorite]
listToVector (nm:ss) (id:ts) (nt:us) (ms:vs) (fl:ws) (yr:xs) (la:ys) (lo:zs) m =
     let meteorite :: Meteorite
         meteorite = Mt {
              meteoriteName = nm,
              meteoriteID = id,
              meteoriteStatus = nt,
              meteoriteMass = ms,
              meteoriteFell = fl,
              meteoriteYear = yr,
              meteoriteLatitude = la,
              meteoriteLongitude = lo
         }
     in listToVector ss ts us vs ws xs ys zs (m ++ [meteorite])

element :: [a] -> a
element []    = error "list too short"
element (x:xs) = x

elementk :: [a] -> Int -> a
elementk [] _     = error "list too short"
elementk (x:xs) 0 = x
elementk (_:xs) k = elementk xs (k - 1)


extractAtributes :: [[Char]] -> [Char] -> [[Char]] -> [[Char]]
extractAtributes [] _ atributes = atributes
extractAtributes (x:xs) meteorite [] =
     let atr :: [[Char]]
         atr = splitOn ":" x
         in let split :: ([Char],[Char])
                split = extraction atr meteorite
                in extractAtributes xs (snd split) [fst split]
extractAtributes (x:xs) meteorite atributes =
     let atr :: [[Char]]
         atr = splitOn ":" x
         in let split :: ([Char],[Char])
                split = extraction atr meteorite
                in extractAtributes xs (snd split) (atributes ++ [fst split])

extraction :: [[Char]] -> [Char] -> ([Char],[Char])
extraction x meteorite =
     let isin :: Bool
         isin = (meteorite =~ (element x))
         in
              if isin == True
                   then
                        let split :: [[Char]]
                            split = splitOn (elementk x 1) (elementk (splitOn (element x) meteorite) 1)
                            in (element split, elementk split 1)
               else ("",meteorite)

{-Extracting attributes from tree-}
mining :: IO ()
mining = do
     input <- readFile "C:/Users/danie/Documents/Daniel/Universidad/4toSemestre/LenguajesFormalesyAutomatas/Lab2/prerows.xml"
     let rows :: [[Char]]
         rows = splitOn "</row>" input
     let atributes :: [[Char]]
         atributes = ["<name>:</name>","<id>:</id>","<nametype>:</nametype>",
                      "<recclass>:</recclass>","<mass>:</mass>","<fall>:</fall>"
                      ,"<year>:</year>","<reclat>:</reclat>",
                      "<reclong>:</reclong>"]
     let atr :: [[Char]]
         atr = extractAtributes atributes (elementk rows 12) []
     print atr
     -- let meteorites :: Vector Meteorite
     --     meteorites = listToVector nm id nt ms fl yr la lo []
     -- Monad.void (encodeMeteoritesToFile "meteoritos.csv" meteorites)
