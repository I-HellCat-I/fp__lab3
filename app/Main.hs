module Main where

import Data.Maybe (mapMaybe)
import Interpolation
import System.Environment (getArgs)
import System.IO (getContents, hPutStrLn, stderr)
import Text.Printf (printf)

-- Парсинг аргументов

parseArgs :: [String] -> Config -> Config
parseArgs [] c = c
parseArgs ("--linear" : xs) c = parseArgs xs (c {cAlgos = Linear : cAlgos c})
parseArgs ("--lagrange" : xs) c = parseArgs xs (c {cAlgos = Lagrange : cAlgos c})
parseArgs ("--newton" : xs) c = parseArgs xs (c {cAlgos = Lagrange : cAlgos c})
parseArgs ("--step" : val : xs) c = parseArgs xs (c {cStep = read val})
parseArgs ("--window" : val : xs) c = parseArgs xs (c {cWindowSize = read val})
parseArgs ("-n" : val : xs) c = parseArgs xs (c {cWindowSize = read val})
parseArgs (_ : xs) c = parseArgs xs c

-- Парсинг ввода и Форматирование вывода

parseLine :: String -> Maybe Point
parseLine line =
  case words (map (\c -> if c == ';' then ' ' else c) line) of
    [x, y] -> Just (read x, read y)
    _ -> Nothing

-- Превращает структуру данных в строку для вывода
formatResult :: InterpolationResult -> String
formatResult (InterpolationResult algo x y) =
  let name = case algo of
        Linear -> "linear"
        Lagrange -> "newton"
   in printf "%s: %g %g" name x y

-- Main

main :: IO ()
main = do
  -- 1. Конфигурация
  args <- getArgs
  let configRaw = parseArgs args defaultConfig
  let config =
        if null (cAlgos configRaw)
          then configRaw {cAlgos = [Linear]}
          else configRaw

  -- 2. Чтение
  inputContent <- getContents
  let points = mapMaybe parseLine (lines inputContent)

  -- 3. Валидация
  let minPoints = if Lagrange `elem` cAlgos config then cWindowSize config else 2

  if length (take minPoints points) < minPoints
    then hPutStrLn stderr "Error: Not enough data points for the selected algorithm."
    else
      -- 4. Запуск логики и потоковая печать
      mapM_ (putStrLn . formatResult) (processStream config points)
