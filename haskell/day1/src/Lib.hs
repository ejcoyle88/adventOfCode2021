module Lib
    ( someFunc
    ) where

countDepthIncreases depths = foldl (\acc (before, current) -> if before < current then acc + 1 else acc) 0 comparisons
  where comparisons = zip ([0] ++ depths) depths

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

makeInteger :: [String] -> [Int]
makeInteger = map read

triZip' [] r = r
triZip' (a:b:[]) r = r ++ [(a,b,0)]
triZip' (a:b:c:xs) r = triZip' ([b,c]++xs) (r ++ [(a,b,c)])

triZip a = triZip' a []

countDepthIncreases2 depths = foldl (\acc ((a,b,c),(d,e,f)) -> if (a+b+c) < (d+e+f) then acc+1 else acc) 0 comparisons
  where zipped = triZip depths
        comparisons = zip ([(0,0,0)] ++ zipped) zipped

someFunc :: IO ()
someFunc = do
  depths <- readLines "input.txt"
  let depthNums = makeInteger depths
  let increases = countDepthIncreases depthNums
  let increases2 = countDepthIncreases2 depthNums
  print (increases - 1)
  print (increases2 - 1)
