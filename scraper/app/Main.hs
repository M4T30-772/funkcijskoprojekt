{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text, strip, unpack)
import Text.HTML.Scalpel
import Data.Char (isDigit)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import System.Exit (exitSuccess)
import Network.URI (parseURI)

data Item = Item
  { name :: Text
  , details :: [Text]
  , timestamp :: String

  } deriving (Show, Eq)

scrapeItems :: String -> IO (Maybe [[Item]])
scrapeItems url = do

  currentTime <- getCurrentTime
  let timestampStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
  scrapeURL url (tables timestampStr)
  where

    tables :: String -> Scraper Text [[Item]]
    tables timestampStr = chroots ("table" @: [hasClass "wikitable"]) (table timestampStr)

    table :: String -> Scraper Text [Item]
    table timestampStr = chroots "tr" (item timestampStr)

item :: String -> Scraper Text Item
item timestampStr = do
  name <- text "th"
  details <- texts "td"
  let clean = strip . strip
  return $ Item (clean name) (map clean details) timestampStr

calculateNumericTotal :: [Text] -> Maybe Int

calculateNumericTotal row
  | all isNumeric strList = Just $ sum $ map readInt strList
  | otherwise = Nothing
  where
    isNumeric str = all isDigit $ stripLeadingSigns $ unpack str
    stripLeadingSigns = dropWhile (\c -> c == '-' || c == '+')
    strList = map strip row
    readInt str = read $ if unpack str == "" then "0" else unpack str

displayItems :: [[Item]] -> IO ()
displayItems items = mapM_ (\x -> mapM_ (\y -> putStrLn (show y) >> putStrLn "") x >> putStrLn "") items

displayItemDetails :: [Item] -> IO ()
displayItemDetails items = mapM_ (\item -> putStrLn ("Name: " ++ show (name item)) >> putStrLn "Details:" >> mapM_ (putStrLn . unpack) (details item) >> putStrLn ("Timestamp: " ++ timestamp item) >> putStrLn "") items

validateURL :: String -> Bool
validateURL url = case parseURI url of
  Just _ -> True
  Nothing -> False

formatItems :: [[Item]] -> String
formatItems items = unlines $ map formatGroup items
  where
    formatGroup :: [Item] -> String
    formatGroup group = unlines $ map formatItem group
    formatItem :: Item -> String
    formatItem item = "Name: " ++ show (name item) ++ "\nDetails:\n" ++ unlines (map ("  " ++) $ map (removeBrackets . show) (details item)) ++ "Timestamp: " ++ timestamp item ++ "\n"
    removeBrackets = init . tail

displayMenu :: IO ()
displayMenu = do

  putStrLn "Choose an option:"
  putStrLn "1. Display the list of items from all tables"
  putStrLn "2. Save the scraped data into scraped_data.txt"
  putStrLn "3. Calculate total number of items scraped"
  putStrLn "4. Display details of a specific item"
  putStrLn "5. Change the URL"
  putStrLn "6. Exit"

main :: IO ()
main = do
  putStrLn "Enter the URL to scrape items from in real time:"
  url <- getLine
  loop url []

loop :: String -> [[Item]] -> IO ()
loop url items = do
  displayMenu
  option <- getLine
  case option of
    "1" -> do
      result <- scrapeItems url
      case result of
        Just x  -> do
          displayItems x
          loop url (x ++ items)
        Nothing -> do
          putStrLn "Didn't find the necessary items."
          loop url items

    "2" -> do
      if validateURL url
        then do
          putStrLn "Saving the scraped data into scraped_data.txt..."
          result <- scrapeItems url
          case result of
            Just x  -> do
              putStrLn "Data saved successfully."
              appendFile "scraped_data.txt" (formatItems x)
              loop url (x ++ items)
            Nothing -> do
              putStrLn "Didn't find the necessary items."
              loop url items
        else do
          putStrLn "Invalid URL."
          newUrl <- getLine
          loop newUrl items

    "3" -> do

      let totalItems = length (concat items)
      putStrLn $ "Total number of items scraped:  " ++ show totalItems
      loop url items

    "4" -> do
      putStrLn "Enter the index of the item to display details (most commonly between 0-4):"
      indexStr <- getLine
      let index = read indexStr :: Int
      if index >= 0 && index < length items
        then do
          let selectedItem = items !! index
          putStrLn "Detils of the selected item:"
          displayItemDetails selectedItem
        else do
          putStrLn "Invalid index"
      loop url items

    "5" -> do
      putStrLn "Enter new URL:"
      newUrl <- getLine
      loop newUrl []

    "6" -> do
      putStrLn "Exiting..."
      exitSuccess
    _   -> do
      putStrLn "Invalid option. Please choose a valid option"
      loop url items
