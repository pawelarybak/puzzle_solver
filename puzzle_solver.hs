import System.Environment


readSolution :: [[(Char, Int)]] -> String
readSolution a = (getKeys . sortByValue . filterUniqueValues . concat) a

getKeys:: [(a, b)] -> [a]
getKeys ((a,b):c) = a:getKeys(c)
getKeys [] = []

sortByValue:: [(a, Int)] -> [(a, Int)]
sortByValue [] = []
sortByValue ((x,i1):xs) = sortByValue (filter (lessTupleValue (x,i1)) xs)
 ++ [(x,i1)]
 ++ sortByValue (filter (moreEqualTupleValue (x,i1)) xs)

lessTupleValue::Ord b =>  (a,b) -> (a,b) -> Bool
lessTupleValue (a,b) (c,d) = b > d

moreEqualTupleValue::Ord b => (a,b) -> (a,b) -> Bool
moreEqualTupleValue (a,b) (c,d) = b <= d


filterUniqueValues::Eq b => [(a, b)] -> [(a, b)]
filterUniqueValues ((a,c):b)
 | (valueElem b c) = (filterUniqueValues b)
 | otherwise = (a,c):filterUniqueValues(b)
filterUniqueValues [] = []

valueElem ::Eq b=> [(a,b)]-> b -> Bool
valueElem ((a,b):c) d = (b == d) || (valueElem c d)
valueElem [] a = False

numerateArrays ::  [[a]] -> Int ->[[(a,Int)]]
numerateArrays ((a):b) c = (numerateRows a c):(numerateArrays b ((length a) + c))
numerateArrays ([]) _ = []
numerateRows ::  [a] -> Int ->[(a, Int)]
numerateRows (a:b) c = (a,c):(numerateRows b (c+1))
numerateRows [] _ = []

transformPuzzle :: String -> [[(Char, Int)]] 
transformPuzzle cont = let numerated = (numerateArrays (lines cont) 1)
                        in numerated 
                         ++ transpose numerated
                         ++ getDiagonals numerated
                         ++ getDiagonals (invert numerated)

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
lowerTriangle ([]) = []

invert :: [a] -> [a]
invert []     = []
invert (x:xs) = (invert xs) ++ [x]

removeWords :: [[(Char, Int)]] -> [Int] -> [[(Char, Int)]]
removeWords puzzles matchedIndexes = map (\puzzle -> filter(\(c, i) -> notElem i matchedIndexes) puzzle) puzzles 

findMatchingIndexes :: [(Char, Int)] -> [Char] -> [Int]
findMatchingIndexes [] _ = []
findMatchingIndexes (x:xs) y = findMatchingIndexes' (x:xs) y [] ++ (findMatchingIndexes xs y)
    where
        findMatchingIndexes' :: [(Char, Int)] -> [Char] -> [Int] -> [Int]
        findMatchingIndexes' _ [] indexes = indexes
        findMatchingIndexes' [] _ _ = []
        findMatchingIndexes' ((x, i):xs) (y:ys) indexes | x == y = findMatchingIndexes' xs ys (indexes ++ [i])
                                                        | otherwise = []


puzzle board words = do
    let strings = transformPuzzle board
    let wordList = lines words
    let matchedIndexes = concatMap (\s -> concatMap (\word -> findMatchingIndexes s word) wordList) strings
    let stringsWithRemovedWords = removeWords strings matchedIndexes
    let solution = readSolution stringsWithRemovedWords
    return solution

main = do
    -- Get files paths from arguments
    args <- getArgs
    let [puzzlePath, wordListPath] = args

    -- Read files
    board <- readFile puzzlePath
    words <- readFile wordListPath

    -- Print board
    putStrLn board

    -- Get and print result
    result <- puzzle board words
    putStrLn result
