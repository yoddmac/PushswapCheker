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
printOK (Just (list_a, list_b)) | not (isSorted (list_a, list_b)) = putStrLn $ "KO: " ++ show (list_a, list_b)
    | otherwise  = putStrLn "OK"

isSorted :: ([Int], [Int]) -> Bool
isSorted ([], [] )= True
isSorted ([_], [_]) = True 
isSorted ([], list_b) = True 
isSorted ([x], []) = True
isSorted (list_a, list_b) | list_a == sort list_a && null list_b = True
                          | otherwise = False

functions :: [String] -> ([Int], [Int]) -> Maybe ([Int], [Int])
functions [] list = Just list
functions (x:xs) list
    | x == "sa" = functions xs (sa list)
    | x == "sb" = functions xs (sb list)
    | x == "sc" = functions xs (sc list)
    | x == "pa" = functions xs (pa list)
    | x == "pb" = functions xs (pb list)
    | x == "ra" = functions xs (ra list)
    | x == "rb" = functions xs (rb list)
    | x == "rr" = functions xs (rr list)
    | x == "rra" = functions xs (rra list)
    | x == "rrb" = functions xs (rrb list)
    | x == "rrr" = functions xs (rrr list)

sa :: ([Int], [Int]) -> ([Int], [Int])
sa ([x], lb) = ([], lb)
sa ([], [] )= ([], []) 
sa ([], list_a) = ([], list_a) 
sa (x:y:xs, lb) = (y:x:xs, lb)

sb :: ([Int], [Int]) -> ([Int], [Int])
sb (la, [x]) = (la, [])
sb ([], [] )= ([], [])
sb (list_b, []) = (list_b, [])
sb (la, x:y:xs) = (la, y:x:xs)

sc :: ([Int], [Int]) -> ([Int], [Int])
sc (list_a, []) = (list_a, [])
sc ([], list_b) = ([], list_b)
sc (a:b:c, d:e:f) = (b:a:c, e:d:f)

pa :: ([Int], [Int]) -> ([Int], [Int])
pa (la, []) = (la, [])
pa (la, y:ys) = (y:la, ys)

pb :: ([Int], [Int]) -> ([Int], [Int])
pb ([], list_a) = ([], list_a)
pb (x:xs, list_a) = (xs, x:list_a)

ra :: ([Int], [Int]) -> ([Int], [Int])
ra ([], list_b) = ([], list_b)
ra (x:xs, lb) = (xs++[x], lb)

rb :: ([Int], [Int]) -> ([Int], [Int])
rb (list_a, []) = (list_a, [])
rb (list_a, x:xs) = (list_a, xs++[x])

rr :: ([Int], [Int]) -> ([Int], [Int])
rr (list_a, []) = (list_a, [])
rr ([], list_b) = ([], list_b)
rr (x:xs, y:ys) = (xs++[x], ys++[y])

rra :: ([Int], [Int]) -> ([Int], [Int])
rra ([], list_b) = ([], list_b)
rra (x:xs, lb) = (last xs:init (x:xs), lb)

rrb :: ([Int], [Int]) -> ([Int], [Int])
rrb (list_b, []) = (list_b, [])
rrb (la, x:xs) = (la, last xs:init (x:xs))

rrr :: ([Int], [Int]) -> ([Int], [Int])
rrr (list_a, []) = (list_a, [])
rrr ([], list_b) = ([], list_b)
rrr (x:xs, y:ys) = (last xs:init (x:xs), last ys:init (y:ys))

main :: IO ()
main = do
    input <- getLine
    args <- getArgs
    let list_a = map read args :: [Int]
    let commands = functions (words input) (list_a, [])
    if isJust commands
        then do 
            printOK commands 
            exitSuccess
        else
            exitWith (ExitFailure 84)