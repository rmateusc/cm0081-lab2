{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Text.XML.HXT.Core

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
listToVector (nm:ss) (id:ts) (nt:us) (ms:vs) (fl:ws) (yr:xs) (la:ys) (lo:zs) _ =
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
     in listToVector ss ts us vs ws xs ys zs (_ ++ [meteorite])

{-Extracting attributes from tree-}
mining :: IO ()
mining = do
     input <- readFile "C:/Users/danie/Documents/Daniel/Universidad/4toSemestre/LenguajesFormalesyAutomatas/Lab2/prerows.xml"
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
     let meteorites :: Vector Meteorite
         meteorites = listToVector nm id nt ms fl yr la lo []
     Monad.void (encodeMeteoritesToFile "meteoritos.csv" meteorites)
