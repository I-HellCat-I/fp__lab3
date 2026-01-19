import Interpolation
import Test.Hspec
import Test.QuickCheck

-- Хелпер для сравнения Double с погрешностью
shouldApprox :: Double -> Double -> Expectation
shouldApprox actual expected =
  actual `shouldSatisfy` (\v -> abs (v - expected) < 0.0001)

main :: IO ()
main = hspec $ do
  describe "Linear Interpolation" $ do
    it "interpolates halfway correctly" $ do
      let pts = [(0, 0), (2, 2)]
      linearInterp pts 1.0 `shouldApprox` 1.0

    it "interpolates negative slopes" $ do
      let pts = [(0, 10), (10, 0)]
      linearInterp pts 5.0 `shouldApprox` 5.0

  describe "Lagrange Interpolation" $ do
    it "interpolates y=x^2 correctly" $ do
      let pts = [(1, 1), (2, 4), (3, 9)]
      -- Точка внутри интервала
      lagrangeInterp pts 2.5 `shouldApprox` 6.25
      -- Экстраполяция (работает, но не требуется заданием)
      lagrangeInterp pts 0.0 `shouldApprox` 0.0

  describe "Process Stream Logic" $ do
    it "generates correct number of steps for Linear" $ do
      let cfg = defaultConfig {cStep = 0.5, cAlgos = [Linear]}
      let pts = [(0, 0), (1, 1), (2, 2)]
      -- Ожидаем точки: 0.0, 0.5, 1.0, 1.5, 2.0 (всего 5)
      let res = processStream cfg pts
      length res `shouldBe` 5
      irY (last res) `shouldApprox` 2.0

  describe "Properties (QuickCheck)" $ do
    it "Linear: returns exact values at nodes" $
      property $
        \x1 y1 x2 y2 ->
          (x1 /= x2) ==>
            let pts = [(x1, y1), (x2, y2)]
             in abs (linearInterp pts x1 - y1) < 0.0001

    -- Проверяем, что Лагранж проходит через ВСЕ узловые точки
    it "Lagrange: returns exact values at all input nodes" $
      property $
        forAll (sortedList 4) $ \pts ->
          all (\(x, y) -> abs (lagrangeInterp pts x - y) < 0.0001) pts

-- Генератор отсортированных точек без дубликатов по X для Лагранжа
sortedList :: Int -> Gen [Point]
sortedList n = do
  x0 <- arbitrary :: Gen Double
  steps <- vectorOf (n - 1) (choose (0.1, 10.0)) :: Gen [Double]
  let xs = scanl (+) x0 steps
  ys <- vectorOf n arbitrary :: Gen [Double]
  return $ zip xs ys
