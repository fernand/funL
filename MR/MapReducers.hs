{-# LANGUAGE OverloadedStrings #-}

module MR.MapReducers (wrappedReducer1, wrappedMapper1) where

import qualified MR.HadoopStreaming as Streaming
import MR.Assert
import MR.Types

import Data.List (sort, nub)
import Data.Aeson
import Control.Monad (mzero)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import qualified Data.ByteString.Lazy.Char8 as B
-- date parsing
import Data.Time.Format
import Data.Time.Clock
import System.Locale (defaultTimeLocale)

data InternalEvent = InternalEvent {ieName :: !String, date :: !Value
    , emitter :: !String, props :: !Object} deriving (Show)
instance FromJSON InternalEvent where
    parseJSON (Object o) = do
        event <- o .: "event"
        pr <- o .: "properties"
        d <- pr .: "time"
        uuid <- pr .: "distinct_id"
        return $ InternalEvent event d uuid pr
    parseJSON _ = mzero

-- 78% of run time for mixpanel events is spent on JSON parsing
wrappedMapper1 :: [Assertion] -> Streaming.Map
wrappedMapper1 assertions row =
    case decode row :: Maybe InternalEvent of
        Nothing -> []
        Just ie -> let reqProperties = getReqProperties (ieName ie) assertions
            in [B.intercalate (B.pack [Streaming.sep])
                [ B.pack $ emitter ie
                , encode $ date ie
                , B.pack $ ieName ie
                , filterProperties reqProperties (props ie)
                ]]

filterProperties :: [T.Text] -> M.HashMap T.Text Value -> B.ByteString
filterProperties req m = encode $ M.filterWithKey (\k v -> elem k req) m

getReqProperties :: String -> [Assertion] -> [T.Text]
getReqProperties event as = nub . concat $ foldPreds grabProperties (map predicate as) where
    grabProperties ei | name ei == event = map fst (properties ei)
    grabProperties _ = []

wrappedReducer1 :: [Assertion] -> Streaming.Reduce
wrappedReducer1 assertions _ vals = case any (==True) results of
    True -> [B.intersperse Streaming.sep $ B.cons 'r' (B.pack $ map toChar results)] 
    False -> []
    where
        events = sort $ map parseEvent vals
        results = assert assertions events

parseEvent :: B.ByteString -> Event
parseEvent b = Event d e where
    fs = fields b
    d = utctDay $ readTime defaultTimeLocale "%s" (B.unpack $ fs !! 0)
    e = EventInfo (B.unpack $ fs !! 1) (getProperties (fs !! 2))

fields :: B.ByteString -> [B.ByteString]
fields b = B.split Streaming.sep b

getProperties :: B.ByteString -> [Property]
getProperties b = case (decode b :: Maybe (M.HashMap T.Text Value)) of
    Just m -> M.toList m
    Nothing -> []

toChar :: Bool -> Char
toChar False = '0'
toChar True = '1'
