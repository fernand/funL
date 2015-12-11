module Main where

import MR.JSON
import MR.HadoopStreaming
import MR.MapReducers
import MR.QueryParser

import System.Environment (getArgs)

main = do
    args <- getArgs
    case head args of
        "--query" -> jPrint parseQuery (args !! 1)
        "--spans" -> jPrint parseForDateSpan (args !! 1)
        "--events" -> jPrint parseForEvents (args !! 1)
        "--mapper1" -> do
            assertions <- jRead "query.json"
            interactMR (mapper $ wrappedMapper1 assertions)
        "--reducer1" -> do
            assertions <- jRead "query.json"
            interactMR (reducer $ wrappedReducer1 assertions)
