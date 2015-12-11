module MR.HadoopStreaming where

import Data.List (groupBy)
import qualified Data.ByteString.Lazy.Char8 as B

type Map = B.ByteString -> [B.ByteString]
type Reduce = B.ByteString -> [B.ByteString] -> [B.ByteString]

type Interactor = B.ByteString -> B.ByteString

sep = '\t'

key :: B.ByteString -> B.ByteString
key = B.takeWhile (/= sep)

value :: B.ByteString -> B.ByteString
value = (B.drop 1) . B.dropWhile (/= sep)

compareKeys :: B.ByteString -> B.ByteString -> Bool
compareKeys a b = (key a) == (key b)

mapper :: Map -> Interactor
mapper mrMap = B.unlines . (concatMap mrMap) . B.lines

reducer :: Reduce -> Interactor
reducer mrReduce input =
    let groups = groupBy compareKeys $ B.lines input in
    B.unlines $ concatMap
        (\g -> mrReduce (key $ head g) (map value g)) groups

interactMR :: Interactor -> IO ()
interactMR = B.interact