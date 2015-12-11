module MR.JSON (jRead, jPrint) where

import Data.Either (rights)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Applicative ((<$>))

jDecode :: FromJSON a => BL.ByteString -> [a]
jDecode bs = case decode bs of
    Nothing -> []
    Just l -> l

jRead :: FromJSON a => String -> IO [a]
jRead s = jDecode <$> (BL.readFile s)

jPrint parser s = BL.putStrLn $ encode $ head . rights $ [parser s]