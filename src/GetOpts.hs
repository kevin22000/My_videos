{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-COT-4-1-wolfram-nicaise.gbenou
-- File description:
-- Conf
-}

module GetOpts (getOpts, nextRow30, nextRow90, rows, rows90, azert, ze, showRows, rule30, rule90, defaultConf) where

import Text.Read ( readMaybe )

data Conf = Conf {
    rule :: Maybe Int
    ,start :: Maybe Int
    ,line :: Maybe Int
    ,window :: Maybe Int
    ,move :: Maybe Int
}

defaultConf :: Conf
defaultConf = Conf {
    rule = Nothing
    ,start = Just 0
    ,line = Nothing
    ,window = Just 0
    ,move = Nothing
}

getOpts :: Conf->[String]->Maybe Conf
getOpts conf [] = Just conf
getOpts conf (x:y:xs) | x == "--rule" = getOpts(conf{rule = (readMaybe y)}) xs
                      | x == "--start" = getOpts(conf{start = (readMaybe y)}) xs
                      | x == "--lines" = getOpts(conf{line = (readMaybe y)}) xs
                      | x == "--window" = getOpts(conf{window = (readMaybe y)}) xs
                      | x == "--move" = getOpts(conf{move = (readMaybe y)}) xs
                      | otherwise = Nothing
getOpts _ _           = Nothing

rule30 :: Bool -> Bool -> Bool -> Bool
rule30 False False False = False
rule30 False False True = True
rule30 False True False = True
rule30 False True True = True
rule30 True False False = True
rule30 True False True = False
rule30 True True False = False
rule30 True True True = False

rule90 :: Bool -> Bool -> Bool -> Bool
rule90 False False False = False
rule90 False False True = True
rule90 False True False = True
rule90 False True True = True
rule90 True False False = True
rule90 True False True = False
rule90 True True False = True
rule90 True True True = False

nextRow30 :: [Bool] -> [Bool]
nextRow30 xs = [rule30(xs !! (i-1)) (xs !! i) (xs !! (i+1)) | i <- [1..length xs - 2]]

nextRow90 :: [Bool] -> [Bool]
nextRow90 xs = [rule90(xs !! (i-1)) (xs !! i) (xs !! (i+1)) | i <- [1..length xs - 2]]

rows :: [Bool] -> [[Bool]]
rows xs = xs : rows(nextRow30 $ replicate padding False ++ xs ++ replicate padding False)
          where padding = 1

rows90 :: [Bool] -> [[Bool]]
rows90 xs = xs : rows90(nextRow90 $ replicate padding False ++ xs ++ replicate padding False)
          where padding = 1

showRows :: [[Bool]] -> String
showRows = unlines.map(map (\b -> if b then '*' else ' '))

azert :: Conf -> IO ()
azert Conf{rule = Just a} | a == 30 = putStr $ showRows $ take 20 $ rows $ replicate 41 False ++ [True] ++ replicate 40 False
                          | a == 90 = putStr $ showRows $ take 20 $ rows90 $ replicate 51 False ++ [True] ++ replicate 50 False
                          | otherwise = putStr("lol")
azert Conf{rule = Nothing} = do
    print ""

ze :: Maybe Conf -> IO ()
ze (Just a) = azert a
ze Nothing = print ""
