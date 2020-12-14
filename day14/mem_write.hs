#!/usr/bin/env stack
-- stack --resolver lts-16.19 script

-- file contents
--
--
import Data.List
import Data.Bits
import qualified Data.Text as T
import Data.Char
import qualified Data.Map.Strict as Map

type Entry = (String, [(Int, Int)])
type Answer = (Int, Int)
type Mem = Map.Map Int Int

testInputs = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X,mem[8] = 11,mem[7] = 101,mem[8] = 0"
main = interact $ showResult . results . map parseRaw . lines

showResult :: Answer -> String
showResult (p1, p2) = "Part I answer: " ++ show p1 ++ ".\nPart II answer: " ++ show p2 ++ ".\n"

parseRaw :: String -> Entry
parseRaw s = let codes = split "," s
             in (drop 7 $ head codes, map str2IntPair $ tail codes)
  where
    str2IntPair s = let (h:values) = split " = " s
                    in (read . filter isDigit $ h, read . head $ values)

results :: [Entry] -> Answer
results es = (foldr (+) 0 $ excute es Map.empty, foldr (+) 0 $ excute2 es Map.empty)
  where
    excute :: [Entry] -> Mem -> Mem
    excute [] m = m
    excute ((mask, codes):es) m = excute es $ foldl' (\m (addr, value) ->
                                                       Map.insert addr (mask `cover` value) m)
                                                     m
                                                     codes
    excute2 :: [Entry] -> Mem -> Mem
    excute2 [] m = m
    excute2 ((mask, codes):es) m = excute2 es $ foldl' (\m' (addr, value) ->
                                                        foldl' (\m'' addr ->
                                                                 Map.insert addr value m'')
                                                               m'
                                                               (floatCover mask addr))
                                                       m
                                                       codes

floatCover mask value = floatCover_ pairs value'
  where
    floatCover_ [] v = [v]
    floatCover_ ((i, c):ms) v = if c == 'X'
                                  then floatCover_ ms (clearBit v i) ++ floatCover_ ms (setBit v i)
                                  else floatCover_ ms v
    value' = foldl' (\v (i, c) -> if c == '1' then setBit v i else v) value pairs
    pairs = zip [0..] $ reverse mask

cover mask value = foldl' (\v (i, c) -> case c of
                                        '0' -> clearBit v i
                                        '1' -> setBit v i
                                        _ -> v) value $ zip [0..] (reverse mask)

split :: String -> String -> [String]
split p = map T.unpack . T.splitOn (T.pack p) . T.pack
