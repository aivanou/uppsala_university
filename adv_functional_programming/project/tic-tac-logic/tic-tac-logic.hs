module Main where

import Control.Monad
import Game

-- Read a board from stdin
readBoard :: Int -> Int -> IO Board
readBoard n m = foldM (\b' i -> liftM (putRow b') (readRow i)) b [1..n]
    where b = emptyBoard n m

-- Read a row from stdin
readRow :: Int -> IO Row
readRow i = do l <- getLine
               let ms = map (\m -> case m of
                                        'X' -> Just X
                                        'O' -> Just O
                                        '.' -> Nothing
                                        _   -> error "Invalid input"
                             ) l
               return $ Row {index = i , marks =  ms}

-- Start the show
main :: IO ()
main = do [n, m] <- fmap ((map (read :: String -> Int)) . words) getLine
          board  <- readBoard n m
          putStrLn $ showGrid $ solve board

