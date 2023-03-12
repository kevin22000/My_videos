{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-COT-4-1-wolfram-nicaise.gbenou
-- File description:
-- Main
-}

module Main (main) where

import System.Environment
import GetOpts

main :: IO ()
main = do 
    azer <- getArgs
    let get = getOpts defaultConf azer
    ze get
