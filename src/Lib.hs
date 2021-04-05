module Lib where

import System.IO

obterString :: IO String 
obterString = do x <- obterChar
                 if x == '\n' then do
                     putChar x
                     return []
                 else
                     if x == '\DEL' then do
                         putStr ['\b', ' ', '\b']
                         return ['\DEL']
                     else do
                         putChar x
                         xs <- obterString
                         if xs == ['\DEL'] then do
                             obterString
                         else
                            return (x:xs)

obterChar:: IO Char 
obterChar = do hSetEcho stdin False
               x <- getChar
               hSetEcho stdin True
               return x