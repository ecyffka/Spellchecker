import WordModel
import WordSuggester

testCorpus = "Oversized monsters are never brought home either alive or preserved, and field measurements are obviously open to doubt because of the universal tendency to exaggerate dimensions. Measurements of skins are of little value; every snake hide is noticeably longer than its carcass and intentional stretching presents no difficulty to the unscrupulous explorer.   In spite of all the pitfalls, there is a certain amount of agreement on some of the giants. The anaconda proves to be the fly in the ointment, but the reason for this is not clear; the relatively wild conditions still found in tropical South America might be responsible.There are three levels on which to treat the subject. The first is the strictly scientific, which demands concrete proof and therefore may err on the conservative side by waiting for evidence in the flesh. This approach rejects virtually all field measurements. The next level attempts to weigh varied evidence and come to a balanced, sensible conclusion; field measurements by experienced explorers are not rejected, and even reports of a less scientific nature are duly evaluated. The third level leans on a belief that a lot of smoke means some fire. The argument against this last approach is comparable to that which rejects stories about hoop snakes, about snakes that break themselves into many pieces and join up again, or even of ghosts that chase people out of graveyards; the mere piling up of testimony does not prove, to the scientific mind, the existence of hoop snakes, joint snakes, or ghosts.   Oliver has recently used the second-level approach with the largest snakes, and has come to these conclusions: the anaconda reaches a length of at least 37 feet, the reticulate python 33, the African rock python 25, the amethystine python at least 22, the Indian python 20, and the boa constrictor 18-1/2.   Bernard Heuvelmans also treats of the largest snakes, but on the third level, and is chiefly concerned with the anaconda. He reasons that as anacondas 30 feet long are often found, some might be 38, and occasional 'monstrous freaks' over 50. He rejects dimensions of 70 feet and more. His thirteenth chapter includes many exciting accounts of huge serpents with prodigious strength, but these seem to be given to complete his picture, not to be believed. Detailed information on record lengths of the giants is given in the section that follows."
tokens = tokenize testCorpus
wm = train (EmptyModel ()) tokens

testWM = do
    file <- readFile "test/testCorpus.txt"
    let wm = train (EmptyModel()) (tokenize file)
    putStr(show wm)

testWM2 = do
    file <- readFile "word-suggestion/brown_nolines.txt"
    let wm = train (EmptyModel()) (tokenize file)
    putStr(show wm)

testProb = do
    file <- readFile "test/testCorpus.txt"
    let wm = train (EmptyModel()) (tokenize file)
    getProb wm

testProb2 = do
    file <- readFile "word-suggestion/brown_nolines.txt"
    let wm = train (EmptyModel()) (tokenize file)
    getProb wm

testSpell = do
    file <- readFile "word-suggestion/brown_nolines.txt"
    let wm = train (EmptyModel()) (tokenize file)
    getSpell wm

getProb wm = do
    putStr("\n What word do you want to check? (Enter 'q' to quit) ")
    w <- getLine
    if w /= "q" 
    then do
        let p = logProb wm w
        putStr(show p)
        getProb wm
    else putStr("done \n")

getSpell wm = do
    putStr("\n What word do you want to check? (Enter 'q' to quit) ")
    w <- getLine
    if w /= "q" 
    then do
        let p = findSuggestions w wm
        putStr(show p)
        getSpell wm
    else putStr("Goodbye \n")