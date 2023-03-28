{-
-- EPITECH PROJECT, 2023
-- B-FUN-400-COT-4-1-wolfram-nicaise.gbenou
-- File description:
-- Conf
-}

module GetOpts (getOpts, nextRow30, nextRow90, nextRow110, rows, rows90, rows110, azert, mine, showRows, rule30, rule90, rule110, defaultConf) where

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
    ,window = Just 80
    ,move = Nothing
}

getOpts :: Conf->[String]->Maybe Conf
getOpts conf [] = Just conf
getOpts conf ("--rule":y:xs) = getOpts(conf{rule =
                        (readMaybe y)}) xs
getOpts conf ("--start":y:xs) = getOpts(conf{start =
                        (readMaybe y)}) xs
getOpts conf ("--lines":y:xs) = getOpts(conf{line = (readMaybe y)}) xs
getOpts conf ("--window":y:xs) = getOpts(conf{window =
                        (readMaybe y)}) xs
getOpts conf ("--move":y:xs) = getOpts(conf{move = (readMaybe y)}) xs
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
rule90 False True False = False
rule90 False True True = True
rule90 True False False = True
rule90 True False True = False
rule90 True True False = True
rule90 True True True = False

rule110 :: Bool -> Bool -> Bool -> Bool
rule110 False False False = False
rule110 False False True = True
rule110 False True False = True
rule110 False True True = True
rule110 True False False = False
rule110 True False True = True
rule110 True True False = True
rule110 True True True = False

rows :: [Bool] -> [[Bool]]
rows xs = xs : rows(nextRow30 $ replicate padding False ++
                    xs ++ replicate padding False)
          where padding = 1

rows90 :: [Bool] -> [[Bool]]
rows90 xs = xs : rows90(nextRow90 $ replicate padding False ++
                        xs ++ replicate padding False)
            where padding = 1

rows110 :: [Bool] -> [[Bool]]
rows110 xs = xs : rows110(nextRow110 $ replicate padding False ++
                            xs ++ replicate padding False)
            where padding = 1

nextRow30 :: [Bool] -> [Bool]
nextRow30 xs = [rule30(xs !! (i-1)) (xs !! i) (xs !! (i+1)) |
                i <- [1..length xs - 2]]

nextRow90 :: [Bool] -> [Bool]
nextRow90 xs = [rule90(xs !! (i-1)) (xs !! i) (xs !! (i+1)) |
                i <- [1..length xs - 2]]

nextRow110 :: [Bool] -> [Bool]
nextRow110 xs = [rule110(xs !! (i-1)) (xs !! i) (xs !! (i+1)) |
                i <- [1..length xs - 2]]

showRows :: [[Bool]] -> String
showRows = unlines.map(map (\b -> if b then '*' else ' '))

azert :: Conf -> IO ()
azert Conf{rule = Just a} | a == 30 = putStr $ showRows $ take 20 $ rows $
                            replicate 41 False ++ [True] ++ replicate 40 False
                          | a == 90 = putStr $ showRows $ take 120 $ rows90 $
                            replicate 41 False ++ [True] ++ replicate 40 False
                          | a == 110 = putStr $ showRows $ take 20 $ rows110 $
                            replicate 41 False ++ [True] ++ replicate 40 False
                          | otherwise = putStr("lol")
azert Conf{rule = Nothing} = print ""

mine :: Maybe Conf -> IO ()
mine (Just a) = azert a
mine Nothing = print ""
