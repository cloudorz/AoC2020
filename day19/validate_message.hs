#!/usr/bin/env stack
-- stack --resolver lts-16.19 script
{-# LANGUAGE OverloadedStrings #-}

-- file contents
--
--
import Data.List
import Data.Bits
import qualified Data.Text as T
import qualified Data.IntMap as M

type Entry = Int
type Answer = (Int, Int)
data Expr =
  P Char
  | N Int
  | Seq Expr Expr
  | Or Expr Expr
  deriving (Show, Eq)

type Env = M.IntMap Expr

changes = ["8: 42 | 42 8", "11: 42 31 | 42 11 31"]

main = do
  content <- getContents
  let (p1:p2:[]) = split "\n\n" content
      exprList = map parseRaw.lines $ p1
      env = M.fromList exprList
      strings = lines p2
      surplusList = map (eval env (N 0)) strings
  putStrLn $ "Passed string count (Part I): " ++ show (passCount surplusList)
  let changesExprs = map parseRaw changes
      updateEnv = foldl1' (.).map (uncurry M.insert) $ changesExprs
      surplusList2 = map (eval (updateEnv env) (N 0)) strings
  putStrLn $ "Passed string count (Part II): " ++ show (passCount surplusList2)
  where
    passCount = length.filter (== "").concat

eval :: Env -> Expr -> String -> [String]
eval env expr vs = eval_ expr vs
  where
    eval_ (P c) (v:vs) = [vs | c == v]
    eval_ _ [] = []
    eval_ (N i) vs = eval_ (env M.! i) vs
    eval_ (Seq e1 e2) vs = concatMap (eval_ e2) (eval_ e1 vs)
    eval_ (Or e1 e2) vs = eval_ e1 vs ++ eval_ e2 vs

parseRaw :: String -> (Int, Expr)
parseRaw s =
  let (p1:p2:[]) = split ": " s
      expr = case p2 of
           ['"', 'a','"'] -> P 'a'
           ['"', 'b','"'] -> P 'b'
           _ -> buildOr $ toIntList p2
  in (read p1, expr)
  where
    toIntList = (fmap.fmap) read.map words.filter (not.null).split " | "
    buildSeq ns = if length ns > 1 then foldl1' Seq . map N $ ns else N . head $ ns
    buildOr nss = if length nss > 1 then foldl1' Or . map buildSeq $ nss else buildSeq.head $ nss

split :: String -> String -> [String]
split p = map T.unpack . T.splitOn (T.pack p) . T.pack
