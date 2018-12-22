readSolution :: [[(Char, Int)]] -> String
readSolution a = (getKeys . sortByValue . filterUniqueKeys . concat) a

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


filterUniqueKeys::Eq a => [(a, b)] -> [(a, b)]
filterUniqueKeys ((a,c):b)
 | (keyElem b a) = (filterUniqueKeys b)
 | otherwise = (a,c):filterUniqueKeys(b)
filterUniqueKeys [] = []

keyElem ::Eq a=> [(a,b)]-> a -> Bool
keyElem ((a,b):c) d = (a == d) || (keyElem c d)
keyElem [] a = False

numerateArrays ::  [[a]] -> Int ->[[(a,Int)]]
numerateArrays ((a):b) c = (numerateRows a c):(numerateArrays b ((length a) + c))
numerateArrays ([]) _ = []
numerateRows ::  [a] -> Int ->[(a, Int)]
numerateRows (a:b) c = (a,c):(numerateRows b (c+1))
numerateRows [] _ = []


puzzle puzzlePath wordListPath = do
    puzzleContent <- (readFile puzzlePath)
    let strings = transformPuzzle puzzleContent
    wordListContent <- readFile wordListPath
    let wordList = lines wordListContent
    return (strings, wordList)

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

removeWords :: [[(Char, Int)]] -> [[Char]] -> [[(Char, Int)]]
removeWords puzzles words = map (\puzzle -> removeWordsFromPuzzle puzzle) puzzles 
    where 
        removeWordsFromPuzzle puzzle = foldl (\acc word -> removeFirst acc word) puzzle words

removeFirst :: [(Char, Int)] -> [Char] -> [(Char, Int)]
removeFirst [] _ = []
removeFirst x [] = x
removeFirst x y = remove x y [] []
    where
        remove :: [(Char, Int)] -> [Char] -> [(Char, Int)] -> [(Char, Int)]-> [(Char, Int)]
        remove x [] _ acc = acc ++ x
        remove [] _ _ acc = acc
        remove ((x, i):xs) (y:ys) underCheck acc | x == y = remove xs ys (underCheck ++ [(x, i)]) acc
                                                 | otherwise = remove xs (y:ys) [] (acc ++ underCheck ++ [(x, i)])