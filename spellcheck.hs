{-
This code is unused in the project but was modified and place in "interaction.hs"
The file is left as evidence of Erin's contribution
-}

{-
This is the main function which gets called from the command line. It prompts the user
for text input, parses it, runs a check against a dictionary, and for each word not found,
provides the user with suggested corrections ("boop" as a placeholder)
-}
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

{-
This splits the user's input into "words" (either actual words or punctuation to
be retaining the final output). Input: lst ([[Char]], the list of words seen so far), 
part ([Char], the word currently being parsed), and the portion of the input string
that has not yet been parsed ([Char])). Output: [[Char]] the list of words contained
in the input string
-}
splitOnSpace :: [[Char]] -> [Char] -> [Char] -> [[Char]]
splitOnSpace lst part "" = if (length part) > 0 then (lst ++ [part]) else lst
splitOnSpace lst part (h:t) = 
    if h /= ' '
        then
            splitOnSpace lst (part ++ [h]) t
    else 
        splitOnSpace (lst ++ [part]) "" t

{-
Checks each word in the list of input words against a preloaded dictionary. If a given
word is not the dictionary, it calls getSuggestions to get suggested correct words.
-}
checkWords :: [[Char]] -> [[Char]] -> [[Char]] -> [[Char]]
checkWords lst _ [] = lst
checkWords lst dict (h:t) =
    if elem h (dict++[",",".","?",['"'],"(",")","!","/",";",":"])
        then checkWords (lst++[h]) dict t
    else
        let correct = getSuggestions h
        in checkWords (lst++[correct]) dict t

{-
This is a placeholder function to call the wordSuggester module for suggestions
for a given word. It currently returns "boop" instead.
-}
getSuggestions :: [Char] -> [Char]
getSuggestions w = "boop"

{-
This puts the correct list of words back together to return to the user.
-}
reassemble :: [[Char]] -> [Char]
reassemble lst =  foldr (\h t -> if not (h `elem` [",",".","?",['"'],"(",")","!","/",";",":"]) then ((' ':h)++t) else (h++t)) "" lst

{-
References:
https://github.com/dwyl/english-words
https://hackage.haskell.org/package/text-2.0.1/docs/Data-Text-IO.html
-}