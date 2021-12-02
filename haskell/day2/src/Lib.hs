module Lib
    ( someFunc
    ) where

handleInputs' :: (Int, Int) -> [[String]] -> Int
handleInputs' (x,z) [] = x*z
handleInputs' (x,z) ([direction, distStr]:xs)
  | direction == "forward" = handleInputs' (x+distance,z) xs
  | direction == "up" = handleInputs' (x,z-distance) xs
  | direction == "down" = handleInputs' (x,z+distance) xs
  | otherwise = handleInputs' (x,z) xs
    where distance = read distStr

handleInputs :: [[String]] -> Int
handleInputs cmds = handleInputs' (0,0) cmds

handleInputsWithAim' :: (Int, Int, Int) -> [[String]] -> Int
handleInputsWithAim' (x,z,aim) [] = x*z
handleInputsWithAim' (x,z,aim) ([direction, distStr]:xs)
  | direction == "forward" = handleInputsWithAim' (x+distance,z+(distance*aim),aim) xs
  | direction == "up" = handleInputsWithAim' (x,z,aim-distance) xs
  | direction == "down" = handleInputsWithAim' (x,z,aim+distance) xs
  | otherwise = handleInputsWithAim' (x,z,aim) xs
    where distance = read distStr

handleInputsWithAim :: [[String]] -> Int
handleInputsWithAim cmds = handleInputsWithAim' (0,0,0) cmds

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

someFunc :: IO ()
someFunc = do
  inputs <- readLines "input.txt"
  let splitInputs = map words inputs
  print (handleInputs splitInputs)
  print (handleInputsWithAim splitInputs)

