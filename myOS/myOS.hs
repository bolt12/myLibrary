-- module MyOS where
module Main where

import Control.Monad
import System.IO 

-- guiao 1 SO --

{-
myCat :: [String] -> IO ()
myCat [] = myCat' [] >> myCat []
    where
        myCat' [] = getChar >>= \c -> if c == '\n' then putStrLn "" else myCat' ([]++[c])
        myCat' l  = getChar >>= \c -> if c == '\n' then putStrLn l else myCat' (l++[c]) 
myCat args = mapM_ (readFile >=> putStr) args

myCat2 :: [String] -> IO ()
myCat2 [] = getLine >>= putStrLn >> myCat2 []
myCat2 args = mapM_ (readFile >=> putStr) args
-}

-- ~10s 
myCatV1 :: IO ()
myCatV1 = do
        hSetBuffering stdout (LineBuffering)
        hSetBuffering stdin (BlockBuffering (Just 1))
        readLoop stdin
        where
            readLoop :: Handle -> IO ()
            readLoop hdl = do
                eof <- hIsEOF hdl
                when ( not eof ) $ do
                    c <- hGetChar hdl
                    hPutChar stdout c
                    readLoop hdl 

-- ~6s
myCatV2 :: Int -> IO ()
myCatV2 bytes = do
        hSetBuffering stdout (BlockBuffering (Just bytes))
        hSetBuffering stdin (BlockBuffering (Just bytes))
        readLoop stdin
        where
            readLoop :: Handle -> IO ()
            readLoop hdl = do
                eof <- hIsEOF hdl
                when ( not eof ) $ do
                    c <- hGetChar hdl
                    hPutChar stdout c
                    readLoop hdl 

-- ~0.3s
myCatV3 :: IO ()
myCatV3 = do
        hSetBuffering stdout (BlockBuffering (Just 1024))
        hSetBuffering stdin (BlockBuffering (Just 1024))
        input <- hGetContents stdin
        hPutStr stdout input

write10mb :: String -> IO ()
write10mb file = do
        fileHdl <- openFile file WriteMode
        write10mbLoop fileHdl (10*1024*1024)
        hClose fileHdl
        where
            write10mbLoop :: Handle -> Int -> IO ()
            write10mbLoop _ 0 = return ()
            write10mbLoop hdl bytes = do
                hPutChar hdl 'a'
                write10mbLoop hdl (bytes-1)

main :: IO()
main = myCatV3
