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
isSorted ([], [] )= True -- si les deux listes sont vides
isSorted ([_], [_]) = False -- jsp mais faut le mettre (ca impact pas le pb)
isSorted ([], list_b) = False -- si la liste a est vide
isSorted ([x], []) = True -- si la liste a ne contient qu'un seul element c'est ici que ca plante pcq le programme passe toujours is meme si y'a plusieurs elements
isSorted (x:y:xs, list_b) = (x <= y) && isSorted (xs, list_b) -- si il y'a au moins deux elements et fonctions de tri

functions :: [String] -> ([Int], [Int]) -> ([Int], [Int])
functions [] (list_a, list_b) = (list_a, list_b)
functions (x:xs) (list_a, list_b)
    | x == "sa" = sa (list_a, list_b)
    | x == "sb" = sb (list_a, list_b)
    | x == "sc" = sc (list_a, list_b)
    | x == "pa" = pa (list_a, list_b)
    | x == "pb" = pb (list_a, list_b)
    | x == "ra" = ra (list_a, list_b)
    | x == "rb" = rb (list_a, list_b)
    | x == "rr" = rr (list_a, list_b)
    | x == "rra" = rra (list_a, list_b)
    | x == "rrb" = rrb (list_a, list_b)
    | x == "rrr" = rrr (list_a, list_b)

sa :: ([Int], [Int]) -> ([Int], [Int])
sa ([x], lb) = ([], lb)
sa (x:y:xs, lb) = (y:x:xs, lb)

sb :: ([Int], [Int]) -> ([Int], [Int])
sb (la, [x]) = (la, [])
sb (la, x:y:xs) = (la, y:x:xs)

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

ra :: ([Int], [Int]) -> ([Int], [Int])
ra ([], list_b) = ([], list_b)
ra (x:xs, lb) = (xs++[x], lb)

rb :: ([Int], [Int]) -> ([Int], [Int])
rb ([], list_b) = ([], list_b)
rb (la, x:xs) = (la, xs++[x])

rr :: ([Int], [Int]) -> ([Int], [Int])
rr (list_a, []) = (list_a, [])
rr ([], list_b) = ([], list_b)
rr (x:xs, y:ys) = (xs++[x], ys++[y])

rra :: ([Int], [Int]) -> ([Int], [Int])
rra ([], list_b) = ([], list_b)
rra (a:b, lb) = (b++[a], lb)

rrb :: ([Int], [Int]) -> ([Int], [Int])
rrb ([], list_b) = ([], list_b)
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
    print commands
    exitSuccess