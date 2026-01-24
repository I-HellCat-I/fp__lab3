import Interpolation
import Test.Hspec
import Test.QuickCheck

shouldApprox :: Double -> Double -> Expectation
shouldApprox actual expected
  | isNaN actual = expectationFailure "Expected value, but got NaN"
  | otherwise = actual `shouldSatisfy` (\v -> abs (v - expected) < 0.0001)

main :: IO ()
main = hspec $ do
  describe "Linear Interpolation" $ do
    it "interpolates halfway" $
      linearInterp [(0, 0), (2, 2)] 1.0 `shouldApprox` 1.0

  describe "Lagrange Interpolation" $ do
    it "interpolates quadratic function" $
      lagrangeInterp [(1, 1), (2, 4), (3, 9)] 2.5 `shouldApprox` 6.25

  describe "Newton Interpolation" $ do
    it "works exactly like Lagrange on 3 points" $ do
      let pts = [(1, 1), (2, 4), (3, 9)]
      newtonInterp pts 2.5 `shouldApprox` 6.25

    it "works on non-equidistant nodes" $ do
      -- y = x^2, точки: 1, 3, 4. Ищем в 2.
      let pts = [(1, 1), (3, 9), (4, 16)]
      newtonInterp pts 2.0 `shouldApprox` 4.0

  describe "Gauss Interpolation" $ do
    it "interpolates on ODD equidistant nodes correctly" $ do
      -- Окно 3 точки: 1, 2, 3 (центр 2). y = x^2
      let pts = [(1, 1), (2, 4), (3, 9)]
      -- Ищем в 2.5 (x > center) -> Forward Gauss logic
      gaussInterp pts 2.5 `shouldApprox` 6.25
      -- Ищем в 1.5 (x < center) -> Backward Gauss logic
      gaussInterp pts 1.5 `shouldApprox` 2.25

    it "returns NaN for even window size" $ do
      let pts = [(1, 1), (2, 4)]
      isNaN (gaussInterp pts 1.5) `shouldBe` True

    it "returns NaN for non-equidistant nodes" $ do
      let pts = [(1, 1), (2, 4), (4, 16)] -- шаг 1, потом 2
      isNaN (gaussInterp pts 2.5) `shouldBe` True