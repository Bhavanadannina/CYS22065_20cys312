import Text.Printf (printf)


oldTaxRegime :: Double -> Double
oldTaxRegime income
  | income <= 2.5 = 0
  | income <= 5 = 0.05 * (income - 2.5)
  | income <= 10 = 0.05 * 2.5 + 0.2 * (income - 5)
  | otherwise = 0.05 * 2.5 + 0.2 * 5 + 0.3 * (income - 10)

newTaxRegime :: Double -> Double
newTaxRegime income
  | income <= 2.5 = 0
  | income <= 5 = 0.05 * (income - 2.5)
  | income <= 7.5 = 0.05 * 2.5 + 0.1 * (income - 5)
  | income <= 10 = 0.05 * 2.5 + 0.1 * 2.5 + 0.15 * (income - 7.5)
  | income <= 12.5 = 0.05 * 2.5 + 0.1 * 2.5 + 0.15 * 2.5 + 0.2 * (income - 10)
  | income <= 15 = 0.05 * 2.5 + 0.1 * 2.5 + 0.15 * 2.5 + 0.2 * 2.5 + 0.25 * (income - 12.5)
  | otherwise = 0.05 * 2.5 + 0.1 * 2.5 + 0.15 * 2.5 + 0.2 * 2.5 + 0.25 * 2.5 + 0.3 * (income - 15)


main :: IO ()
main = do
  putStrLn "Enter your annual income (in rupees):"
  incomeInput <- getLine
  let incomeInLakhs = (read incomeInput :: Double) / 100000

  putStrLn "Choose the tax regime:"
  putStrLn "1. Old Tax Regime"
  putStrLn "2. New Tax Regime"
  regimeInput <- getLine

  let tax = case regimeInput of
              "1" -> oldTaxRegime incomeInLakhs
              "2" -> newTaxRegime incomeInLakhs
              _   -> error "Invalid choice. Please select 1 or 2."

  printf "Your calculated tax is: ₹%.2f\n" (tax * 100000) 

