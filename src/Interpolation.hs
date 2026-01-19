module Interpolation
  ( Point,
    AlgoType (..),
    Config (..),
    InterpolationResult (..),
    defaultConfig,
    processStream,
    linearInterp, -- <--- ДОБАВЛЕНО: нужно для тестов
    lagrangeInterp, -- <--- ДОБАВЛЕНО: нужно для тестов
  )
where

import Data.List (find)
import Data.Maybe (fromMaybe)

-- ==========================================
-- Типы данных
-- ==========================================

type Point = (Double, Double)

data AlgoType = Linear | Lagrange
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
-- Математика
-- ==========================================

linearInterp :: [Point] -> Double -> Double
linearInterp ((x0, y0) : (x1, y1) : _) x =
  y0 + (x - x0) * (y1 - y0) / (x1 - x0)
linearInterp _ _ = 0 / 0

lagrangeInterp :: [Point] -> Double -> Double
lagrangeInterp points x = sum $ zipWith (curry term) points [0 ..]
  where
    term ((xi, yi), i) = yi * product [(x - xj) / (xi - xj) | ((xj, _), j) <- zip points [0 ..], i /= j]

calc :: AlgoType -> [Point] -> Double -> Double
calc Linear pts x = linearInterp (take 2 pts) x
calc Lagrange pts x = lagrangeInterp pts x

-- ==========================================
-- Потоковая логика (Sliding Window)
-- ==========================================

processStream :: Config -> [Point] -> [InterpolationResult]
processStream cfg inputStream =
  let wSize = if Lagrange `elem` cAlgos cfg then cWindowSize cfg else 2
   in go wSize [] inputStream Nothing
  where
    step = cStep cfg
    algos = cAlgos cfg

    go :: Int -> [Point] -> [Point] -> Maybe Double -> [InterpolationResult]
    -- 1. Накопление окна (если окно не заполнено и есть данные)
    go wSize window (p : ps) nextX
      | length window < wSize = go wSize (window ++ [p]) ps nextX
    -- 2. Обработка окна
    go wSize window inputStream nextX =
      let -- Безопасное получение X из начала окна
          headWindowX = case window of
            ((x, _) : _) -> x
            [] -> 0.0 -- Недостижимо при корректной логике
          currX = fromMaybe headWindowX nextX

          hasMoreData = not (null inputStream)
          limitIndex = if hasMoreData then wSize `div` 2 else wSize - 1

          -- Безопасное получение границы
          limitX = fst (window !! limitIndex)

          pointsToCalc = takeWhile (<= limitX + 1e-9) [currX, currX + step ..]

          results =
            concatMap
              ( \xVal ->
                  map (\algo -> InterpolationResult algo xVal (calc algo window xVal)) algos
              )
              pointsToCalc

          newNextX = if null pointsToCalc then currX else last pointsToCalc + step

          -- Рекурсивный вызов: сдвиг окна
          nextStep = case inputStream of
            (newP : restInput) ->
              -- drop 1 безопаснее tail
              let newWindow = drop 1 window ++ [newP]
               in go wSize newWindow restInput (Just newNextX)
            [] -> [] -- Данных больше нет
       in results ++ nextStep
