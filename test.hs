--
-- EPITECH PROJECT, 2021
-- B-FUN-300-NCY-3-1-pushswapchecker-hugo.valdenaire
-- File description:
-- main
--

import Data.List
import Data.Maybe
import Data.Char
import System.IO
import System.Environment
import System.Directory
import System.Exit

printOK :: Maybe ([Int], [Int]) -> IO()
printOK (Just (list_a, list_b)) | not (isSorted (list_a, list_b)) = putStrLn $ "KO :"++(show (list_a, list_b))
    | otherwise  = putStrLn "OK"

isSorted :: ([Int], [Int]) -> Bool
isSorted ([], list_b) = True
isSorted (x:y:xs, list_b) = (x <= y) && isSorted (xs, list_b)

functions :: [String] -> ([Int], [Int]) -> Maybe ([Int], [Int])
functions [] (list_a, list_b) = Just (list_a, list_b)
functions (x:xs) (list_a, list_b) = Nothing


main :: IO ()
main = do
    input <- getLine
    args <- getArgs
    let list_a = map read args :: [Int]
    let commands = functions (words input) (list_a, [])
    printOK commands
    exitSuccess
    --isSorted (commands) == False then Print "KO"