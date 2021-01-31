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

printOK :: ([Int], [Int]) -> IO()
printOK (list_a, list_b) | not (isSorted (list_a, list_b)) = putStrLn $ "KO :" ++ show (list_a, list_b)
    | otherwise  = putStrLn "OK"

isSorted :: ([Int], [Int]) -> Bool
isSorted ([], [] )= True
isSorted ([_], [_]) = False 
isSorted ([], list_b) = False 
isSorted ([x], []) = True
isSorted (list_a, list_b) | list_a == sort (list_a) && list_b == [] = True
                          | otherwise = False

functions :: [String] -> ([Int], [Int]) -> ([Int], [Int])
functions [] (list) = (list)
functions (x:xs) (list)
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
sa (list_a, []) = (list_a, [])
sa (x:y:xs, lb) = (y:x:xs, lb)

sb :: ([Int], [Int]) -> ([Int], [Int])
sb ([], [] )= ([], [])
sb ([_], [_]) = ([], [])
sb ([], list_b) = ([], [])
sb ([x], []) = ([], [])
sb (list_b, []) = (list_b, [])
sb (la, [x]) = (la, [])
sb (la, x:y:xs) = (la, y:x:xs)

sc :: ([Int], [Int]) -> ([Int], [Int])
sc (list_a, []) = (list_a, [])
sc ([], list_b) = ([], list_b)
sc (a:b:c, d:e:f) = (b:a:c, e:d:f)

pa :: ([Int], [Int]) -> ([Int], [Int])
pa ([], [] )= ([], []) 
pa ([_], [_]) = ([], []) 
pa ([], list_a) = ([], list_a) 
pa ([x], []) = ([], [])
pa (list_a, []) = (list_a, [])
pa (x:xs, y:ys) = (y:(x:xs), ys)

pb :: ([Int], [Int]) -> ([Int], [Int])
pb ([], [] )= ([], [])
pb ([_], [_]) = ([], [])
pb ([], list_b) = ([], [])
pb ([x], []) = ([], [])
pb (list_b, []) = (list_b, [])
pb (x:xs, y:ys) = (xs, x:(y:ys))

ra :: ([Int], [Int]) -> ([Int], [Int])
ra ([], list_b) = ([], list_b)
ra (x:xs, lb) = (xs++[x], lb)

rb :: ([Int], [Int]) -> ([Int], [Int])
rb ([], list_b) = ([], list_b)
rb ([_], [_]) = ([], [])
rb ([x], []) = ([], [])
rb (list_b, []) = (list_b, [])
rb (la, x:xs) = (la, xs++[x])

rr :: ([Int], [Int]) -> ([Int], [Int])
rr (list_a, []) = (list_a, [])
rr ([], list_b) = ([], list_b)
rr (x:xs, y:ys) = (xs++[x], ys++[y])

rra :: ([Int], [Int]) -> ([Int], [Int])
rra ([], list_b) = ([], list_b)
rra (a:b, lb) = (b++[a], lb)

rrb :: ([Int], [Int]) -> ([Int], [Int])
rrb ([], [] )= ([], [])
rrb ([_], [_]) = ([], [])
rrb ([], list_b) = ([], [])
rrb ([x], []) = ([], [])
rrb (list_b, []) = (list_b, [])
rrb (la, b:a) = (la, a++[b])

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