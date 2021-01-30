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
printOK (Just (list_a, list_b)) | not (isSorted (list_a, list_b)) = putStrLn $ "KO :" ++ show (list_a, list_b)
    | otherwise  = putStrLn "OK"

isSorted :: ([Int], [Int]) -> Bool
isSorted ([], list_b) = True
isSorted (x:y:xs, list_b) = (x <= y) && isSorted (xs, list_b)

functions :: [String] -> ([Int], [Int]) -> Maybe ([Int], [Int])
functions [] (list_a, list_b) = Just (list_a, list_b)
functions (x:xs) (list_a, list_b)
    | x == "sa" = functions xs (list_a, list_b)
    | x == "sb" = functions xs (list_a, list_b)
    | x == "sc" = functions xs (list_a, list_b)
    | x == "pa" = functions xs (list_a, list_b)
    | x == "pb" = functions xs (list_a, list_b)
    | x == "ra" = functions xs (list_a, list_b)
    | x == "rb" = functions xs (list_a, list_b)
    | x == "rr" = functions xs (list_a, list_b)

sa :: [Int] -> [Int]
sa [] = []
sa (a:b:c) = b:a:c

sb :: [Int] -> [Int]
sb [] = []
sb (a:b:c) = b:a:c

sc :: ([Int], [Int]) -> ([Int], [Int])
sc (list_a, []) = (list_a, [])
sc ([], list_b) = ([], list_b)
sc (a:b:c, d:e:f) = (b:a:c, e:d:f)

pa :: ([Int], [Int]) -> ([Int], [Int])
pa (list_a, []) = (list_a, [])
pa (x:xs, y:ys) = (y:(x:xs), ys)

pb :: ([Int], [Int]) -> ([Int], [Int])
pb ([], list_b) = ([], list_b)
pb (x:xs, y:ys) = (xs, x:(y:ys))

ra :: [Int] -> [Int]
ra [] = []
ra (x:xs) = xs++[x]

rb :: [Int] -> [Int]
rb [] = []
rb (x:xs) = xs++[x]

rr :: ([Int], [Int]) -> ([Int], [Int])
rr (list_a, []) = (list_a, [])
rr ([], list_b) = ([], list_b)
rr (x:xs, y:ys) = (xs++[x], ys++[y])

rra :: [Int] -> [Int]
rra [] = []
rra (a:b) = b++[a]

rrb :: [Int] -> [Int]
rrb [] = []
rrb (b:a) = a++[b]

rrr :: ([Int], [Int]) -> ([Int], [Int])
rrr (list_a, []) = (list_a, [])
rrr ([], list_b) = ([], list_b)
rrr (a:b, c:d) = (b++[a], d++[c])


main :: IO ()
main = do
    input <- getLine
    args <- getArgs
    let list_a = map read args :: [Int]
    let commands = functions (words input) (list_a, [])
    printOK commands
    exitSuccess
    --isSorted (commands) == False then Print "KO"
    --functions_sa (commands) == False then Print "KO"