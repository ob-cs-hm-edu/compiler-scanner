module Main where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import Data.Char (isDigit)

data Token = NatNum Integer | OpeningParen | ClosingParen | Add | Mult
    deriving (Show)

scan :: String -> Maybe [Token]
scan ""         = Just []
scan (' ' :xs)  = scan xs
scan ('\t':xs)  = scan xs
scan ('(' :xs)  = fmap (\tokens -> OpeningParen:tokens) $ scan xs
scan (')' :xs)  = fmap (\tokens -> ClosingParen:tokens) $ scan xs
scan ('+' :xs)  = fmap (\tokens -> Add         :tokens) $ scan xs
scan ('*' :xs)  = fmap (\tokens -> Mult        :tokens) $ scan xs
scan str@(x:xs)
    | isDigit x =
        let (digits, rest) = span isDigit str
        in fmap (\tokens -> NatNum (read digits):tokens)
                $ scan rest
    | otherwise = Nothing

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    input <- getLine
    maybe (putStrLn "error") print $ scan input
    main

