module Interpolation
  ( Point,
    AlgoType (..),
    Config (..),
    InterpolationResult (..),
    defaultConfig,
    processStream,
    linearInterp,
    lagrangeInterp,
    newtonInterp,
    gaussInterp,
  )
where

import Data.List (foldl')
import Data.Maybe (fromMaybe)

-- ==========================================
-- Типы данных
-- ==========================================

type Point = (Double, Double)

data AlgoType = Linear | Lagrange | Newton | Gauss
  deriving (Show, Eq)

data Config = Config
  { cStep :: Double,
    cWindowSize :: Int,
    cAlgos :: [AlgoType]
  }
  deriving (Show)

defaultConfig :: Config
defaultConfig =
  Config
    { cStep = 1.0,
      cWindowSize = 4,
      cAlgos = []
    }

data InterpolationResult = InterpolationResult
  { irAlgo :: AlgoType,
    irX :: Double,
    irY :: Double
  }
  deriving (Show)

-- ==========================================
-- Вспомогательная математика
-- ==========================================

factorial :: Int -> Double
factorial n = product [1 .. fromIntegral n]

-- Безопасная проверка на равномерность
isEquidistant :: [Point] -> Bool
isEquidistant pts =
  let xs = map fst pts
   in case xs of
        (x0 : x1 : _) ->
          let h = x1 - x0
              diffs = zipWith (-) (drop 1 xs) xs
           in all (\d -> abs (d - h) < 1e-9) diffs
        _ -> True -- 0 или 1 точка считаются равномерными

-- Безопасные конечные разности
finiteDifferences :: [Double] -> [[Double]]
finiteDifferences ys =
  let go currentLevel
        | length currentLevel < 2 = [currentLevel]
        | otherwise =
            let nextLevel = zipWith (-) (drop 1 currentLevel) currentLevel
             in currentLevel : go nextLevel
   in go ys

-- ==========================================
-- Алгоритмы Интерполяции
-- ==========================================

-- 1. Линейная
linearInterp :: [Point] -> Double -> Double
linearInterp ((x0, y0) : (x1, y1) : _) x =
  y0 + (x - x0) * (y1 - y0) / (x1 - x0)
linearInterp _ _ = 0 / 0

-- 2. Лагранж
lagrangeInterp :: [Point] -> Double -> Double
lagrangeInterp points x = sum $ zipWith (curry term) points [0 ..]
  where
    term ((xi, yi), i) = yi * product [(x - xj) / (xi - xj) | ((xj, _), j) <- zip points [0 ..], i /= j]

-- 3. Ньютон
newtonInterp :: [Point] -> Double -> Double
newtonInterp [] _ = 0 / 0
newtonInterp points targetX =
  let xs = map fst points
      ys = map snd points
      coeffs = dividedDifferences xs ys
   in evalNewton xs coeffs targetX

-- Безопасное вычисление разделенных разностей
dividedDifferences :: [Double] -> [Double] -> [Double]
dividedDifferences xs ys =
  let (finalAcc, finalYs) = foldl' step ([], ys) [1 .. length xs - 1]
      -- Безопасное получение последнего коэффициента (вершины)
      lastCoeff = case finalYs of
        (c : _) -> c
        [] -> 0.0 -- Не должно случаться при корректной логике
   in reverse (lastCoeff : finalAcc)
  where
    step (acc, currentYs) k =
      case currentYs of
        (c_current : _) ->
          let yPairs = zip currentYs (drop 1 currentYs)
              xPairs = zip xs (drop k xs)

              nextYs =
                zipWith
                  (\(y0, y1) (x0, x1) -> (y1 - y0) / (x1 - x0))
                  yPairs
                  xPairs
           in (c_current : acc, nextYs)
        [] -> (acc, []) -- Защита от пустого списка

evalNewton :: [Double] -> [Double] -> Double -> Double
evalNewton nodes coeffs x = sum $ zipWith term coeffs [0 ..]
  where
    term c k = c * product (take k [x - xi | xi <- nodes])

-- 4. Гаусс
gaussInterp :: [Point] -> Double -> Double
gaussInterp pts x
  | null pts = 0 / 0
  | even (length pts) = 0 / 0
  | not (isEquidistant pts) = 0 / 0
  | otherwise =
      let n = length pts - 1
          alphaInd = n `div` 2

          -- Безопасное вычисление h
          h = case pts of
            ((x0, _) : (x1, _) : _) -> x1 - x0
            _ -> 1.0
          ys = map snd pts
          finDifs = finiteDifferences ys

          dts1 = [0, -1, 1, -2, 2, -3, 3, -4, 4]
          centerX = fst (pts !! alphaInd)

          calcTerm k signFunc =
            let indices = take k dts1
                termMult = product [(x - centerX) / h + fromIntegral (if signFunc then j else (-j)) | j <- indices]
                row = finDifs !! k
                midIdx = length row `div` 2
                idx =
                  if signFunc
                    then midIdx
                    else midIdx - (1 - length row `mod` 2)
                deltaVal = row !! idx
             in termMult * deltaVal / factorial k

          f1 = (ys !! alphaInd) + sum [calcTerm k True | k <- [1 .. n]]
          f2 = (ys !! alphaInd) + sum [calcTerm k False | k <- [1 .. n]]
       in if x > centerX then f1 else f2

-- Диспетчер
calc :: AlgoType -> [Point] -> Double -> Double
calc Linear pts x = linearInterp (take 2 pts) x
calc Lagrange pts x = lagrangeInterp pts x
calc Newton pts x = newtonInterp pts x
calc Gauss pts x = gaussInterp pts x

-- ==========================================
-- Потоковая логика
-- ==========================================

processStream :: Config -> [Point] -> [InterpolationResult]
processStream cfg inputStream =
  let reqWindow = cWindowSize cfg
      wSize =
        if any (`elem` [Lagrange, Newton, Gauss]) (cAlgos cfg)
          then reqWindow
          else 2
   in go wSize [] inputStream Nothing
  where
    step = cStep cfg
    algos = cAlgos cfg

    go :: Int -> [Point] -> [Point] -> Maybe Double -> [InterpolationResult]
    go wSize window (p : ps) nextX
      | length window < wSize = go wSize (window ++ [p]) ps nextX
    go wSize window inputStream nextX =
      let headWindowX = case window of ((x, _) : _) -> x; [] -> 0.0
          currX = fromMaybe headWindowX nextX

          hasMoreData = not (null inputStream)
          limitIndex = if hasMoreData then wSize `div` 2 else wSize - 1
          limitX = fst (window !! limitIndex)

          pointsToCalc = takeWhile (<= limitX + 1e-9) [currX, currX + step ..]

          results =
            concatMap
              ( \xVal ->
                  map (\algo -> InterpolationResult algo xVal (calc algo window xVal)) algos
              )
              pointsToCalc

          newNextX = if null pointsToCalc then currX else last pointsToCalc + step

          nextStep = case inputStream of
            (newP : restInput) ->
              let newWindow = drop 1 window ++ [newP]
               in go wSize newWindow restInput (Just newNextX)
            [] -> []
       in results ++ nextStep
