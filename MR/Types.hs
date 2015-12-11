{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module MR.Types where

import Data.Time.Calendar (Day)
import System.Locale (defaultTimeLocale)
import Data.Time.Format
import Data.List (intersect)
import Data.Maybe (maybeToList)
import Control.Applicative ((<$>))
import Control.Monad (mzero)
import qualified Data.Text as T
import GHC.Generics
import Data.Aeson

instance ToJSON Day where
    toJSON d = object [ "day"  .= show d ]
instance FromJSON Day where
    parseJSON (Object o) = day <$> o .: "day"
        where day s = head . maybeToList $ parseTime defaultTimeLocale "%Y-%m-%d" s
    parseJSON _ = mzero

data DateSpan = DateSpan (Maybe Day) (Maybe Day) deriving (Eq,Show,Ord, Generic)
instance ToJSON DateSpan
instance FromJSON DateSpan

data Interval = AllSpan | Days Int | Weeks Int | Months Int | Quarters Int | Years Int
    deriving (Eq, Show, Ord, Generic)
instance ToJSON Interval
instance FromJSON Interval

type Property = (T.Text, Value)

data EventInfo = EventInfo {name :: String, properties :: [Property]}
                 deriving (Eq, Show, Generic)
instance Ord EventInfo where
    compare (EventInfo n _) (EventInfo m _) | n/=m = GT
    compare (EventInfo _ ps) (EventInfo _ qs) |
        length (intersect ps qs) == length ps = LT
    compare _ _ = GT
instance ToJSON EventInfo
instance FromJSON EventInfo

data Event = Event {day :: Day, info :: EventInfo}
    deriving (Eq, Show)
instance Ord Event where
    compare (Event d _) (Event d' _) = compare d d'

data Predicate = And Predicate Predicate | Seq Predicate Predicate |
                   Or Predicate Predicate | Nil | Single EventInfo
    deriving (Show, Generic)
instance ToJSON Predicate
instance FromJSON Predicate

foldPreds :: (EventInfo -> a) -> [Predicate] -> [a]
foldPreds f [] = []
foldPreds f (Nil:ps) = foldPreds f ps
foldPreds f ((Single ei):ps) = (f ei):(foldPreds f ps)
foldPreds f ((And p1 p2):ps) = foldPreds f (p1:p2:ps)
foldPreds f ((Or p1 p2):ps) = foldPreds f (p1:p2:ps)
foldPreds f ((Seq p1 p2):ps) = foldPreds f (p1:p2:ps)

data Assertion = Assertion {interval :: Interval, predicate :: Predicate
    , dateSpan :: DateSpan} 
    deriving (Show, Generic)
instance ToJSON Assertion
instance FromJSON Assertion
