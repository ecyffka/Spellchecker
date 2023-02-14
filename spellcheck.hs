
main = do
    dictionaryString <- readFile "dictionary.txt"
    let dictionary = lines dictionaryString
    putStrLn "Enter text below"
    input <- getLine
    let addSpaces = foldr (\h t -> if h `elem` [',','.','?','"','(',')','!','/',';',':'] then (' ':h:t) else (h:t)) "" input
    putStrLn (show addSpaces)
    let split = splitOnSpace [] "" addSpaces
    putStrLn (show split)
    let corrected = checkWords split
    return (tail (reassemble corrected))

splitOnSpace :: [[Char]] -> [Char] -> [Char] -> [[Char]]
splitOnSpace lst part "" = if (length part) > 0 then (lst ++ [part]) else lst
splitOnSpace lst part (h:t) = 
    if h /= ' '
        then
            splitOnSpace lst (part ++ [h]) t
    else 
        splitOnSpace (lst ++ [part]) "" t

checkWords :: [[Char]] -> [[Char]]
checkWords (h:t) = (h:t)

reassemble :: [[Char]] -> [Char]
reassemble lst =  foldr (\h t -> if not (h `elem` [",",".","?",['"'],"(",")","!","/",";",":"]) then ((' ':h)++t) else (h++t)) "" lst

-- https://github.com/dwyl/english-words
-- https://hackage.haskell.org/package/text-2.0.1/docs/Data-Text-IO.html