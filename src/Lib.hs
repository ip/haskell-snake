module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = myIo 0

myIo :: Integer -> IO ()
myIo 5 = return ()
myIo i = putStrLn (show i) >>
        myIo (i + 1)
