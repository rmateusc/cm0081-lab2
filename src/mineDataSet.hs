{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Text.Regex.TDFA
import Data.List.Split
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Control.Exception (IOException)
import qualified Control.Monad as Monad
import Data.ByteString.Lazy (ByteString)
import qualified Data.Foldable as Foldable
import qualified Control.Exception as Exception
import qualified Data.ByteString.Lazy as ByteString

import Data.Csv
  ( DefaultOrdered(headerOrder)
  , Header
  , ToField(toField)
  , ToNamedRecord(toNamedRecord)
  , (.=)
  )
import qualified Data.Csv as Cassava

{- | Represents a Meteorite. With name, id, status, mass, fell, year, latitude
and longitude as attributes. -}
data Meteorite =
     Mt
    {
        meteoriteName :: String,
        meteoriteID :: String,
        meteoriteStatus :: String,
        meteoriteMass :: String,
        meteoriteFell :: String,
        meteoriteYear :: String,
        meteoriteLatitude :: String,
        meteoriteLongitude :: String
    }

{- | Displays attributes of Meteorite. -}
instance Show Meteorite where
     show (Mt nm di nt ms fl yr la lo) =
          show nm ++ "," ++ show di ++ "," ++ show nt ++ "," ++ show ms ++ ","
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

{- | This function recieves a meteorite vector and encodes it into a
ByteString. -}
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

{- | This function recieves a list of strings and a list of meteorites and
transforms them into a vector of meteorites. -}
listToVector :: [[String]] -> [Meteorite]
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
element (x:_) = x

elementk :: [a] -> Int -> a
elementk [] _     = error "list too short"
elementk (x:_) 0 = x
elementk (_:xs) k = elementk xs (k - 1)

{- | This function takes the first 4 elements of a string. -}
toYear :: String -> String
toYear = take 4

{- | This function replaces a string depending on how it is. -}
goodOrBad :: String -> String
goodOrBad "Valid" = "Good"
goodOrBad "Relict" = "Bad"
goodOrBad _ = ""

extraction :: [String] -> String -> (String,String)
extraction attr meteorite =
     let isin :: Bool
         isin = (meteorite =~ element attr)
         in if isin
                then if element attr == "<nametype>"
                        then let splitt :: [String]
                                 splitt = splitOn (elementk attr 1) (elementk (splitOn (element attr) meteorite) 1)
                              in (goodOrBad (element splitt), elementk splitt 1)
                         else if element attr == "<year>"
                            then let splitt :: [String]
                                     splitt = splitOn (elementk attr 1) (elementk (splitOn (element attr) meteorite) 1)
                              in (toYear (element splitt), elementk splitt 1)
                         else let splitt :: [String]
                                  splitt = splitOn (elementk attr 1) (elementk (splitOn (element attr) meteorite) 1)
                                  in (element splitt, elementk splitt 1)
               else ("",meteorite)

extractAttributes :: [String] -> String -> [String] -> [String]
extractAttributes [] _ attributes = attributes
extractAttributes (x:xs) meteorite [] =
     let attr :: [String]
         attr = splitOn ":" x
         in let splitt :: (String,String)
                splitt = extraction attr meteorite
                in extractAttributes xs (snd splitt) [fst splitt]
extractAttributes (x:xs) meteorite attributes =
     let attr :: [String]
         attr = splitOn ":" x
         in let splitt :: (String,String)
                splitt = extraction attr meteorite
               in extractAttributes xs (snd splitt) (attributes ++ [fst splitt])

listOfMeteorites :: [String] -> [String] -> [[String]] -> [[String]]
listOfMeteorites [] _ meteorite = meteorite
listOfMeteorites (x:xs) attributes [] =
     let meteorite :: [String]
         meteorite = extractAttributes attributes x []
         in listOfMeteorites xs attributes [meteorite]
listOfMeteorites (x:xs) attributes meteorite =
     let met :: [String]
         met = extractAttributes attributes x []
         in listOfMeteorites xs attributes (meteorite ++ [met])

main :: IO ()
main = do
     input <- readFile "prerows.xml"
     let rows :: [String]
         rows = splitOn "</row>" input
     let attributes :: [String]
         attributes = ["<name>:</name>","<id>:</id>","<nametype>:</nametype>",
                      "<recclass>:</recclass>","<mass>:</mass>","<fall>:</fall>"
                      ,"<year>:</year>","<reclat>:</reclat>",
                      "<reclong>:</reclong>"]
     let met :: [[String]]
         met = listOfMeteorites rows attributes []
     let meteorites :: Vector Meteorite
         meteorites = listToVector met []
     Monad.void (encodeMeteoritesToFile "meteoritos.csv" meteorites)
