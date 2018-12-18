


puzzle puzzlePath wordListPath = do
    puzzleContent <- (readFile puzzlePath)
    let strings = transformPuzzle puzzleContent
    wordListConent <- readFile wordListPath
    let wordList = lines wordListConent
    return (strings, wordList)

transformPuzzle :: String -> [String]
transformPuzzle cont = lines cont 
                        ++ transpose (lines cont) 
                        ++ getDiagonals (lines cont)
                        ++ getDiagonals (invert (lines cont))

getDiagonals :: [[a]] -> [[a]]
getDiagonals str = [mainDiagonal str] ++ lowerTriangle str ++ lowerTriangle (transpose str)

transpose :: [[a]]-> [[a]]
transpose ([]:_) = []
transpose x      = (map head x) : transpose (map tail x)

mainDiagonal :: [[a]] -> [a]
mainDiagonal []      = []
mainDiagonal ([]:xs) = []
mainDiagonal (x:xs)  = head x : mainDiagonal (map tail xs)

lowerTriangle :: [[a]] -> [[a]]
lowerTriangle (x:y:[]) = [[head y]]
lowerTriangle (x:xs)   = (mainDiagonal xs):(lowerTriangle xs)

invert :: [a] -> [a]
invert []     = []
invert (x:xs) = (invert xs) ++ [x]