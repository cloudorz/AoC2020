#!/usr/bin/env stack
-- stack --resolver lts-16.19 script
{-# LANGUAGE OverloadedStrings #-}

-- file contents
--
--
import Data.List
import Data.Bits
import qualified Data.Text as T

type Entry = String
type Answer = (Int, Int)

data BTree a = Empty | Node (BTree a) a (BTree a)
  deriving (Eq, Ord, Show)

type Expr = BTree Char

main = interact $ showResult . results . map parseRaw . lines

showResult :: Answer -> String
showResult (p1, p2) = "Part I answer: " ++ show p1 ++ ".\nPart II answer: " ++ show p2 ++ ".\n"

parseRaw :: String -> Entry
parseRaw = filter (/= ' ')

results :: [Entry] -> Answer
results es = (psum exprTree, psum exprTree2)
  where
    psum f = sum $ map (computeExpr . f) es

computeExpr :: Expr -> Int
computeExpr (Node Empty c Empty) = read [c]
computeExpr (Node l c r) = (if c == '*' then (*) else (+)) (computeExpr l)  (computeExpr r)

exprTree2 :: String -> Expr
exprTree2 = exprTree_ Empty
  where
    exprTree_ bt [] = bt
    exprTree_ bt css@(c:cs) =
      case c of
        '(' -> let (pcs, scs) = partStr css
               in if bt == Empty
                    then exprTree_ (exprTree_ Empty pcs) scs
                    else let Node l a _ = bt
                         in if a == '*'
                              then Node l a (exprTree_ (exprTree_ Empty pcs) scs)
                              else exprTree_ (Node l a (exprTree_ Empty pcs)) scs
        '*' -> exprTree_ (Node bt '*' Empty) cs
        '+' -> exprTree_ (Node bt '+' Empty) cs
        _ -> if bt == Empty
               then exprTree_ (Node Empty c Empty) cs
               else let Node l a _ = bt
                    in if a == '*'
                         then Node l a (exprTree_ (Node Empty c Empty) cs)
                         else exprTree_ (Node l a (Node Empty c Empty)) cs

exprTree :: String -> Expr
exprTree = exprTree_ Empty
  where
    exprTree_ bt [] = bt
    exprTree_ bt css@(c:cs) =
      case c of
        '(' -> let (pcs, scs) = partStr css
               in if bt == Empty
                    then exprTree_ (exprTree_ Empty pcs) scs
                    else let Node l a _ = bt
                         in exprTree_ (Node l a (exprTree_ Empty pcs)) scs
        '*' -> exprTree_ (Node bt '*' Empty) cs
        '+' -> exprTree_ (Node bt '+' Empty) cs
        _ -> if bt == Empty
               then exprTree_ (Node Empty c Empty) cs
               else let Node l a _ = bt
                    in exprTree_ (Node l a (Node Empty c Empty)) cs

partStr :: String -> (String, String)
partStr s = partStr_ [] s 0
  where
    partStr_ pcs scs n
      | length pcs > 0 && n <= 0 = (pcs, scs)
      | otherwise = let (c:cs) = scs
                    in case c of
                      '(' -> partStr_ (if n >= 1 then pcs ++ [c] else pcs) cs (n+1)
                      ')' -> partStr_ (if n - 1 <= 0 then pcs else pcs ++ [c]) cs (n-1)
                      _ -> partStr_ (pcs ++ [c]) cs n
