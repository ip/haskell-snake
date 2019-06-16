module Lib
    ( someFunc
    ) where

someFunc :: IO ()

{--
-- A REPL which multiplies numbers by 2
someFunc = runStep

runStep :: IO ()
runStep = do
    putStrLn "Enter a number or \"q\" to quit: "
    line <- getLine
    if line == "q" then
        return ()
    else do
        let x = read line :: Integer
            in putStrLn $ show x ++ " * 2 = " ++ (show $ x * 2)
        runStep
--}

if' :: Bool -> a -> a -> a
if' True  x y = x
if' False x y = y

someFunc =
    putStrLn $ if' True "Yes" "No"
