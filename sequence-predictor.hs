import System.IO
import Data.Text (Text, unpack)
import qualified Data.Text as T

add x y =  x + y

fwdDiff f n = f (n+1) - f n


getNthElement :: [Int] -> Int -> Int
getNthElement list n
  | n >= 0 && n < length list = list !! n
  | otherwise = error "index out of bounds"


fallingFactorial :: Int -> Int -> Int
fallingFactorial k n
  | k < 0     = error "k must be a non-negative integer"
  | k == 0    = 1
  | otherwise = product [n, n - 1..n - k + 1]

factorial :: Int -> Int
factorial n = product [1..n]

umbralTaylorSeries :: Int -> (Int -> Int) -> Int -> Int -> Double
umbralTaylorSeries n f x l
  | n >= l    = 0
  | otherwise = term + umbralTaylorSeries (n + 1) f x l
  where
    term = (fromIntegral (composeNTimes n fwdDiff f 0) / fromIntegral (factorial n)) * fromIntegral (fallingFactorial n x)

composeNTimes :: Int -> (a -> a) -> a -> a
composeNTimes n f x
  | n == 0    = x
  | otherwise = foldr1 (.) (replicate n f) x

splitEach :: T.Text -> [T.Text] -> [[T.Text]]
splitEach delimiter = map (T.splitOn delimiter)
textListToIntList :: [[Text]] -> [[Int]]
textListToIntList = map (map (read . T.unpack))


firstNumber :: [Int] -> Int
firstNumber var = round $ umbralTaylorSeries 0  (getNthElement var) (-1) (length var)

lastNumber :: [Int] -> Int
lastNumber var = round $ umbralTaylorSeries 0  (getNthElement var) (length var) (length var)

main = do
  contents <- readFile "input"
  let lst = lines contents
  let resultList = map (T.splitOn (T.pack " ")) (map T.pack lst)
  let intList = textListToIntList resultList
  let finalList=map lastNumber intList
  print finalList
