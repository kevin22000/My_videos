module Main (main) where

import System.Environment
import GetOpts

main :: IO ()
main = do 
    azer <- getArgs
    let get = getOpts defaultConf azer
    ze get
