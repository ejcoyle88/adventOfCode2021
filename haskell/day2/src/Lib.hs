module Lib
    ( someFunc
    ) where

handleInputs' :: (Int, Int) -> [[String]] -> Int
handleInputs' (x,z) [] = x*z
handleInputs' (x,z) ([direction, distStr]:xs) = 
  if direction == "forward" then handleInputs' (x+distance,z) xs
  else
    if direction == "up" then handleInputs' (x,z-distance) xs
    else handleInputs' (x,z+distance) xs
  where distance = read distStr

handleInputs :: [[String]] -> Int
handleInputs cmds = handleInputs' (0,0) cmds

handleInputsWithAim' :: (Int, Int, Int) -> [[String]] -> Int
handleInputsWithAim' (x,z,aim) [] = x*z
handleInputsWithAim' (x,z,aim) ([direction, distStr]:xs) =
  if direction == "forward" then handleInputsWithAim' (x+distance,z+(distance*aim),aim) xs
  else
    if direction == "up" then handleInputsWithAim' (x,z,aim-distance) xs
    else handleInputsWithAim' (x,z,aim+distance) xs
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

