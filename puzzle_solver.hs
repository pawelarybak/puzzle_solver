


puzzleIn path = do cont <- readFile path 
                   let values = lines cont
                   return values

