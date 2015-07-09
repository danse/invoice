{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

import Text.Hastache
import Text.Hastache.Context
import Data.Data

import Data.Text.Lazy.Builder
import Data.Text.Lazy.IO
import Data.Text.Encoding as E
import Data.ByteString hiding (map)
import Data.ByteString.Lazy hiding (map)
import Data.ByteString.Lazy.Char8 hiding (map)
import Data.Aeson
import GHC.Generics
import Data.Data
import Data.Time.Clock
import System.Locale
import Data.Time.Format

data Week = Week {
  interval :: String,
  description :: String,
  hours :: Float
  } deriving (Data, Typeable, Generic)

instance FromJSON Week
instance ToJSON Week

data Table = Table {
  weeks :: [Week],
  project :: String
  } deriving (Data, Typeable, Generic)

instance FromJSON Table
instance ToJSON Table

data Content = Content {
  tables :: [Table],
  dollarsPerHour :: Float,
  eurosPerHour :: Float,
  contact :: [Line],
  client :: [Line],
  contactBank :: [Line],
  reference :: Integer
  } deriving (Data, Typeable, Generic)

instance FromJSON Content
instance ToJSON Content

data Line = Line {
  line :: String
  } deriving (Data, Typeable, Generic)

instance FromJSON Line
instance ToJSON Line

data TransformedTable = TransformedTable {
  table :: Table,
  total :: Float
  } deriving (Data, Typeable, Generic)

instance FromJSON TransformedTable
instance ToJSON TransformedTable

data Calculated = Calculated {
  transformedTables :: [TransformedTable],
  totalHours :: Float,
  totalEuros :: Float,
  date :: UTCTime
  } deriving (Data, Typeable, Generic)

instance FromJSON Calculated
instance ToJSON Calculated

tableHours :: Table -> Float
tableHours = sum . (map hours) . weeks

sumAllHours :: Content -> Float
sumAllHours = sum . (map tableHours) . tables

sumAllEuros :: Content -> Float
sumAllEuros content = (eurosPerHour content) * (sumAllHours content)

transformTable :: Table -> TransformedTable
transformTable table = TransformedTable {
  table = table,
  total = tableHours table
  }

-- transform :: Content -> UTCTime -> _
transform content time = composeCtx contentCtx calculatedCtx
  where contentCtx = mkGenericContext content
        calculatedCtx = mkGenericContext calculated
        calculated = Calculated {
          transformedTables = map transformTable $ tables content,
          totalHours = sumAllHours content,
          totalEuros = sumAllEuros content,
          date = time
          }

render = hastacheFileBuilder defaultConfig "template.html"
  
main = do
  contentJSON <- Data.ByteString.Lazy.readFile "content.json"
  time <- getCurrentTime
  case (eitherDecode contentJSON :: Either String Content) of
    Left error -> do
      Prelude.putStrLn error
    Right content -> do
      res <- render (transform content time)
      Data.Text.Lazy.IO.writeFile "invoice.html" (toLazyText res)
  
