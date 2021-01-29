--
-- EPITECH PROJECT, 2021
-- PushSwap
-- File description:
-- Checker
--

import System.Environment   
import Data.List  

main = do  
    args <- getArgs
    mapM putStrLn args