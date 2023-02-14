main :: IO [Char]
main = do
    dictionaryString <- readFile "dictionary.txt"
    let dictionary = lines dictionaryString
    putStrLn "Enter text below"
    input <- getLine
    let addSpaces = foldr (\h t -> if h `elem` [',','.','?','"','(',')','!','/',';',':'] then (' ':h:t) else (h:t)) "" input
    let split = splitOnSpace [] "" addSpaces
    let corrected = checkWords [] dictionary split
    return (tail (reassemble corrected))

splitOnSpace :: [[Char]] -> [Char] -> [Char] -> [[Char]]
splitOnSpace lst part "" = if (length part) > 0 then (lst ++ [part]) else lst
splitOnSpace lst part (h:t) = 
    if h /= ' '
        then
            splitOnSpace lst (part ++ [h]) t
    else 
        splitOnSpace (lst ++ [part]) "" t

checkWords :: [[Char]] -> [[Char]] -> [[Char]] -> [[Char]]
checkWords lst _ [] = lst
checkWords lst dict (h:t) =
    if elem h (dict++[",",".","?",['"'],"(",")","!","/",";",":"])
        then checkWords (lst++[h]) dict t
    else
        let correct = getSuggestions h
        in checkWords (lst++[correct]) dict t

getSuggestions :: [Char] -> [Char]
getSuggestions w = "boop"

reassemble :: [[Char]] -> [Char]
reassemble lst =  foldr (\h t -> if not (h `elem` [",",".","?",['"'],"(",")","!","/",";",":"]) then ((' ':h)++t) else (h++t)) "" lst

-- https://github.com/dwyl/english-words
-- https://hackage.haskell.org/package/text-2.0.1/docs/Data-Text-IO.html