module Main where

import Data.Maybe (mapMaybe)
import Interpolation
import System.Environment (getArgs)
import System.IO (getContents, hPutStrLn, stderr)
import Text.Printf (printf)

-- Парсинг аргументов
parseArgs :: [String] -> Config -> Config
parseArgs [] c = c
-- Алгоритмы
parseArgs ("--linear" : xs) c = parseArgs xs (c {cAlgos = Linear : cAlgos c})
parseArgs ("--lagrange" : xs) c = parseArgs xs (c {cAlgos = Lagrange : cAlgos c})
parseArgs ("--newton" : xs) c = parseArgs xs (c {cAlgos = Newton : cAlgos c})
parseArgs ("--gauss" : xs) c = parseArgs xs (c {cAlgos = Gauss : cAlgos c})
-- Параметры
parseArgs ("--step" : val : xs) c = parseArgs xs (c {cStep = read val})
parseArgs ("--window" : val : xs) c = parseArgs xs (c {cWindowSize = read val})
parseArgs ("-n" : val : xs) c = parseArgs xs (c {cWindowSize = read val})
parseArgs (_ : xs) c = parseArgs xs c

-- Парсинг строки данных
parseLine :: String -> Maybe Point
parseLine line =
  case words (map (\c -> if c == ';' then ' ' else c) line) of
    [x, y] -> Just (read x, read y)
    _ -> Nothing

formatResult :: InterpolationResult -> String
formatResult (InterpolationResult algo x y) =
  let name = case algo of
        Linear -> "linear"
        Lagrange -> "lagrange"
        Newton -> "newton"
        Gauss -> "gauss"

      -- Обработка ошибок (например, Gauss на четном окне вернет NaN)
      valStr =
        if isNaN y
          then "NaN"
          else printf "%g" y
   in printf "%s: %g %s" name x valStr

main :: IO ()
main = do
  args <- getArgs
  let configRaw = parseArgs args defaultConfig
  -- Дефолтный алгоритм - Linear
  let config =
        if null (cAlgos configRaw)
          then configRaw {cAlgos = [Linear]}
          else configRaw

  inputContent <- getContents
  let points = mapMaybe parseLine (lines inputContent)

  -- Определяем минимально необходимое кол-во точек
  let minPoints =
        if any (`elem` [Lagrange, Newton, Gauss]) (cAlgos config)
          then cWindowSize config
          else 2

  -- Проверка на валидность данных для Гаусса (предупреждение)
  if Gauss `elem` cAlgos config && even (cWindowSize config)
    then hPutStrLn stderr "Warning: Gauss algorithm requires an ODD window size (e.g., -n 3 or -n 5). Results will be NaN."
    else return ()

  if length (take minPoints points) < minPoints
    then hPutStrLn stderr "Error: Not enough data points."
    else mapM_ (putStrLn . formatResult) (processStream config points)