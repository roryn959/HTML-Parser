import System.IO
import System.Exit

main = do
    handle <- openFile "file.html" ReadMode
    contents <- hGetContents handle
    evaluateContents contents
    hClose handle

evaluateContents :: String -> IO()
evaluateContents contents = do
    let tokenStream = lex' [] "" False False contents
    --print tokenStream
    parse tokenStream

parse :: [String] -> IO()
parse tokenStream = do
    if (parse' tokenStream) then print "Parsing successful."
    else print "Parsing failed."

listSlice :: [a] -> Int -> Int -> [a]
listSlice xs i j = take (j-i+1) (drop i xs)

strip :: [a] -> [a]
strip xs = init (tail xs)

tagList :: [String]
tagList = ["html", "head", "body", "title", "h1", "h2", "h3", "p", "div", "ul", "li", "a"]

validTag :: String -> Bool
validTag "br" = True
validTag "hr" = True
validTag tag = (tag `elem` tagList) || ( tag `elem` (map (\x -> '/':x) tagList) )

lex' :: [String] -> String -> Bool -> Bool -> String -> [String]
--If at EOF, check if still in a tag. If so, if needs to be closed. Else, return tokens
lex' tokenStream currentTag inTag inAttributes "" = if inTag then error "Missing a '>' - unclosed tag" else tokenStream
--If we see '<', should not be in a tag. Start paying attention to characters. Should not be in attributes yet
lex' tokenStream currentTag inTag inAttributes ('<':xs) = 
    if inTag then
        error "'<' should not appear in tag definition"
    else lex' tokenStream "" True False xs
--If we see '>', no longer in tag or attributes.
lex' tokenStream currentTag inTag inAttributes ('>':xs) = 
    if not inTag then 
        error "'>' should only appear to close a tag" 
    else if validTag currentTag then
        lex' (tokenStream++[currentTag]) "" False False xs
    else error ("Could not match the tag <" ++ currentTag ++ ">")
--If we meet a space inside a tag, we are now in the attributes section. Should not be inAttributes
lex' tokenStream currentTag True False (' ':xs) = lex' tokenStream currentTag True True xs
--If we are in a tag and in attributes, ignore them
lex' tokenStream currentTag True True (x:xs) = lex' tokenStream currentTag True True xs
--If in tag but not attributes, we need to keep track of the characters
lex' tokenStream currentTag True False (x:xs) = lex' tokenStream (currentTag++[x]) True False xs
--If not in a tag, just ignore character
lex' tokenStream currentTag False inAttributes (x:xs) = lex' tokenStream currentTag False inAttributes xs

parse' :: [String] -> Bool
parse' tokenStream = if ( (head tokenStream) == "html" ) && ( (last tokenStream) == "/html") then checkInner (strip tokenStream) else error "File must be entirely wrapped in <html> tags"

checkInner :: [String] -> Bool
checkInner tokenStream = (nested tokenStream []) && (checkHeadBody tokenStream False False) && (runThrough tokenStream False False)

--Check if tags are correctly nested
nested :: [String] -> [String] -> Bool
nested [] stack = if stack /= [] then error ("Unclosed tag <" ++ stack!!0 ++ ">") else True
nested ("br":tokenStream) stack = nested tokenStream stack
nested ("hr":tokenStream) stack = nested tokenStream stack
nested (token:tokenStream) stack = 
    if token!!0 == '/' then 
        if stack == [] then 
            error ("Unexpected closing tag <" ++ token ++ ">")
        else --Attempt pop from stack
            if (tail token) == (last stack) then
                nested tokenStream (init stack)
            else error ("Unexpected closing tag <" ++ token ++ ">")
    else nested tokenStream (stack++[token])

checkHeadBody :: [String] -> Bool -> Bool -> Bool
checkHeadBody [] headSeen bodySeen = if not (headSeen && bodySeen) then error "Must be both a head and a body section defined" else True
checkHeadBody ("head":tokenStream) headSeen bodySeen = if headSeen then error "Only one head section may be defined" else checkHeadBody tokenStream False bodySeen
checkHeadBody ("/head":tokenStream) headSeen bodySeen = checkHeadBody tokenStream True bodySeen
checkHeadBody ("body":tokenStream) headSeen bodySeen = if not headSeen then error "Head must be defined before body." else checkHeadBody tokenStream headSeen bodySeen
checkHeadBody ("/body":tokenStream) headSeen bodySeen = if bodySeen then error "Only one body section may be defined" else checkHeadBody tokenStream headSeen True
checkHeadBody (token:tokenStream) headSeen bodySeen = checkHeadBody tokenStream headSeen bodySeen

runThrough :: [String] -> Bool -> Bool -> Bool
runThrough [] inHead inP = True
runThrough ("title":tokenStream) inHead inP = if inHead then runThrough tokenStream inHead inP else error "Title may only be defined in the head section."
runThrough ("head":tokenStream) inHead inP = runThrough tokenStream True inP
runThrough ("/head":tokenStream) inHead inP = runThrough tokenStream False inP
runThrough ("p":tokenStream) inHead inP = if inP then error "<p> tags may not be nested within eachother." else runThrough tokenStream inHead True
runThrough ("/p":tokenStream) inHead inP = runThrough tokenStream inHead False
runThrough ("div":tokenStream) inHead inP = if inP then error "<div> tags may not be nested inside <p> tags." else runThrough tokenStream inHead inP
runThrough (token:tokenStream) inHead inP = runThrough tokenStream inHead inP