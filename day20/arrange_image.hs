#!/usr/bin/env stack
-- stack --resolver lts-16.19 script
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- file contents
--
--
import Data.List
import Data.Bits
import qualified Data.Text as T
import Numeric (showIntAtBase)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, fromJust)

type Content = [String] -- 10x10 Char matrix

type Tile = (Int, Content)

type Answer = (String, String)

type Point = (Int, Int)

type Image = M.Map Point Tile

type AdjacentMatrix = [[Int]]

data EdgeType = L|T|R|B

main = do
  contents <- getContents
  let tiles = map parseRaw.map lines.split "\n\n" $ contents
      matrix = matrixFromTiles tiles
      imageIDList = fst $ unzip tiles
  putStrLn ("Tile ID: " ++ show imageIDList)
  putStrLn $ showMatrix matrix
  let answerOfPart1 = mulFourCornerIDs (zip (map sum matrix) imageIDList)
  let chaosImage = rearrangeTiles matrix tiles
      orderImage = adjustTiles chaosImage
      mergedImage = jointImage orderImage
  putStrLn $ "Origin Chaos Tiles: \n" ++ showImage chaosImage
  putStrLn $ "Adjust Tiles: \n" ++ showImage orderImage
  putStrLn $ "Joint Image:\n" ++ showTile mergedImage
  let (tile, positions) = searchMonster monster mergedImage
      monsterTile = markMonster tile positions
      countOfSharp = countSharp monsterTile
  putStrLn $ "Monster show: \n" ++ showTile monsterTile
  putStrLn ("Part I: " ++ show answerOfPart1)
  putStrLn ("Part II: " ++ show countOfSharp)
  where
    mulFourCornerIDs = product.snd.unzip.take 4.sort


showResult :: Answer -> String
showResult (p1, p2) = "Part I answer: " ++ p1 ++ ".\nPart II answer: " ++ p2 ++ ".\n"

parseRaw :: [String] -> Tile
parseRaw (n:es) = (read.delete ':'.drop 5 $ n, es)

-- Sea Monster
type Monster = [String]

monster :: Monster
monster = [
  "                  # ",
  "#    ##    ##    ###",
  " #  #  #  #  #  #   "]


searchMonster :: Monster -> Tile -> (Tile, [Point])
searchMonster monster tile =
  let tiles = sequence.fmap allVariantContents $ tile
      findAllM = map (findMonsterPositions monster.snd)
  in head.filter (not.null.snd) $ zip tiles (findAllM tiles)

findMonsterPositions :: Monster -> Content -> [Point]
findMonsterPositions monster content = checkPoint (0, 0) []
  where
    (!tileH, !tileW) = sizeOfContent $ content
    (!mH, !mW) = (length monster, length.head $ monster)
    !dm = digitalMonster monster
    checkPoint (ox, oy) points =
      if ox + mH > tileH then
        points
      else if oy + mW > tileW then
             checkPoint (succ ox, 0) points
           else let mPoints = map (plusP (ox, oy)) dm
                in if all (== '#') (map (getPixelAt content) mPoints)
                      && intersect mPoints points == []
                     then
                       checkPoint (ox, succ oy) (mPoints ++ points)
                     else checkPoint (ox, succ oy) points

markMonster :: Tile -> [Point] -> Tile
markMonster tile points = fmap markContent tile
  where
    markContent content = map mapRow (zip [0..] content)
    mapRow (row, rvs) = map (\(col, v) ->
                               if (row, col) `elem` points then 'O' else v)
                            (zip [0..] rvs)

digitalMonster :: Monster -> [Point]
digitalMonster = concatMap (\(row, ps) -> map ((,) row) $ elemIndices '#' ps)
                           . zip [0..]

-- Image
imageToMatrix :: Image -> [[Tile]]
imageToMatrix image = group values
  where
    values = M.elems image
    size = round.sqrt.fromIntegral.length $ values
    group [] = []
    group values = let (pres, succs) = splitAt size values in pres:group succs

jointImage :: Image -> Tile
jointImage image =
  let ((row, col), _) = M.findMax image
      contentMatrix = (fmap.fmap) (cutFourEdges.snd).imageToMatrix $ image --[[ cutFourEdges.snd $ image M.! (row', col') | col' <- [0..col]] | row' <- [0..row]]
      content = concatMap (foldl1' (zipWith (++))) contentMatrix
  in (0, content)

adjustTiles :: Image -> Image
adjustTiles image = M.mapWithKey convertTile image
  where
    convertTile point tile =
      let tiles = nearTiles point
          variants = sequence $ fmap allVariantContents tile
      in fromJust $ find (and.zipWith compareT tiles.getFourEdges.snd) variants
    nearTiles point = map ((image M.!?).plusP point) fourDirection
    fourDirection = [(0, 1), (1, 0), (0, -1), (-1, 0)]
    compareT tile edge = fromMaybe True $ fmap (edgeRelateTo edge) tile

 -- (0, 1) Right (0, -1) Left (1, 0) Down (-1, 0) Up
 -- snake recusion base on adjacent martix
rearrangeTiles :: AdjacentMatrix -> [Tile] -> Image
rearrangeTiles !matrix ts =
  let orderIndices = arrange_ ((0, 0), fstMinIndex, (0, 1)) matrix M.empty
  in M.map (ts !!) orderIndices
  where
    arrange_ :: (Point, Int, Point) -> AdjacentMatrix -> M.Map Point Int -> M.Map Point Int
    arrange_ (point, index, offset) matrix cache =
      let indices = elemIndices 1 $ getRowAt matrix index
          newCache = M.insert point index cache
      in case length indices of
           0 -> newCache
           n -> let newOffset = if n == 1 then rotateP 90 offset else offset
                    nextIndex = if n == 1 || point == (0, 0)
                      then head indices
                      else let pps@((d1, _):(d2, _):ps) = zip (map (sum . getColAt matrix) indices) indices
                           in if length ps > 0 then
                              error ("exceed, degree > 2: " ++ show pps)
                              else if d1 == d2 then
                                     let nearRow = cache M.! (rotateP (-45) offset `plusP` point)
                                     in head $ filter ((== 1) . getValueAt matrix nearRow) indices
                                   else snd $ minimum pps
                in arrange_ (point `plusP` newOffset, nextIndex, newOffset)
                            (setColValueAt index 0 . setValueAt index nextIndex 0 $ matrix) -- reduce the martix
                            newCache
    !fstMinIndex = snd . minimum $ zip (map sum matrix) [0..]

showImage :: Image -> String
showImage image =
  let ((row, col), _) = M.findMax image
      !tilesMatrix = [[ image M.! (row', col') | col' <- [0..col]] | row' <- [0..row]]
      !ids = unlines.map (intercalate " ").(fmap.fmap) (show.fst) $ tilesMatrix
      contents = unlines.map (unlines.foldl1' (zipWith (\s1 s2 -> s1++" "++s2)).snd.unzip) $ tilesMatrix
  in "IDs :\n" ++ ids ++ "\n\nContents: \n" ++ contents

-- Matrix
matrixFromTiles :: [Tile] -> AdjacentMatrix
matrixFromTiles ts = [[if t1 `relateTo` t2 then 1 else 0 | t1 <- ts] | t2 <- ts]

getRowAt :: AdjacentMatrix -> Int -> [Int]
getRowAt m i = m !! i

getColAt :: AdjacentMatrix -> Int -> [Int]
getColAt m j = transpose m !! j

getValueAt :: AdjacentMatrix -> Int -> Int -> Int
getValueAt m i j = (m !! i) !! j

setValueAt :: Int -> Int -> Int -> AdjacentMatrix -> AdjacentMatrix
setValueAt i j v m = map (\(i', ns) ->
                           map (\(j', n) ->
                             if [i', j'] \\ [i, j] == [] then v else n)
                             (zip [0..] ns))
                         (zip [0..] m)

setColValueAt :: Int -> Int -> AdjacentMatrix -> AdjacentMatrix
setColValueAt j v m = map (\ns ->
                           map (\(j', n) ->
                             if j' == j then v else n)
                             (zip [0..] ns))
                          m

showMatrix :: AdjacentMatrix -> String
showMatrix = unlines.map (intercalate " ".map show)

-- Image content
getVariantEdges :: Content -> [String] -- [Top, Bottom, Left, Bottom, flip ones]
getVariantEdges content =
  let origin = getFourEdges content
  in origin ++ map reverse origin

getPixelAt :: Content -> Point -> Char
getPixelAt content (row, col) = (content !! row) !! col

sizeOfContent :: Content -> (Int, Int)
sizeOfContent content = (length content, length.head $ content)

cutFourEdges :: Content -> Content
cutFourEdges content =
  transpose . init . tail . transpose . init . tail $ content

getEdge :: EdgeType -> Content -> String
getEdge t content =
  case t of
    L -> head $ transpose content
    R -> last $ transpose content
    T -> head content
    B -> last content

getFourEdges :: Content -> [String] -- [Right, Bottom, Left, Top]
getFourEdges content = map (flip getEdge content) [R, B, L, T]

allVariantContents :: Content -> [Content]
allVariantContents content =
  let getFourContents = take 4 . iterate rotateContent
  in getFourContents content ++ getFourContents (flipContent content)

rotateContent :: Content -> Content -- clockwise
rotateContent = flipContent . transpose

flipContent :: Content -> Content
flipContent = map reverse

-- Tile
countSharp = countFor '#'

countMonster = countFor 'O'

countFor :: Char -> Tile -> Int
countFor c (_, content) = sum.map (\e -> if e == c then 1 else 0).concat $ content

relateTo :: Tile -> Tile -> Bool
relateTo (n1, b1) (n2, b2) = n1 /= n2 && length intersectEdges > 0
  where
    intersectEdges = getVariantEdges b1 `intersect` getVariantEdges b2

edgeRelateTo :: String -> Tile -> Bool
edgeRelateTo edge tile = edge `elem` (getVariantEdges . snd $ tile)

showTile :: Tile -> String
showTile (n, content) = "ID No. " ++ show n ++ ":\n" ++ unlines content

-- help

split :: String -> String -> [String]
split p = map T.unpack . T.splitOn (T.pack p) . T.pack

plusP :: Point -> Point -> Point
plusP (x, y) (x', y') = (x+x', y+y')

-- x2 = cos * x1 + sin * y1, y2 = cos * y1 - sin * x1
rotateP :: Int -> (Int, Int) -> (Int, Int)
rotateP degree (x, y) =
  let p = fromIntegral degree/180*pi
  in (round(cos p * fromIntegral x + sin p * fromIntegral y),
      round(cos p * fromIntegral y - sin p * fromIntegral x))
