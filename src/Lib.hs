module Lib
    ( someFunc
    ) where

someFunc :: IO ()
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
