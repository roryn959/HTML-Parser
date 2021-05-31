import System.IO

listSlice :: [a] -> Int -> Int -> [a]
listSlice xs i j = take (j-i+1) (drop i xs)

findTagIndex :: (Eq a) => [a] -> a -> Int -> Int
findTagIndex [] x n = undefined
findTagIndex (x:xs) y n = if x==y then n else findTagIndex xs y (n+1)

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

--parse :: [String] -> Bool
--parse tokenStream = html tokenStream

--html :: [String] -> Bool
--html tokenStream = ((head tokenStream) == "html") && ((last tokenStream) == "/html") && (main (strip tokenStream))

title :: [String] -> Bool
title ["title", "/title"] = True
title xs = terminal xs

terminal :: [String] -> Bool
terminal [] = True
terminal ["br"] = True
terminal ["hr"] = True
terminal ["p", "/p"] = True
terminal ["h1", "/h1"] = True
terminal ["h2", "/h2"] = True
terminal ["h3", "/h3"] = True
terminal ["a", "/a"] = True
terminal xs = False

list_element :: [String] -> Bool
list_element xs = ((head xs) == "li") && ((last xs) == "/li") && ( (terminal $ strip xs) || (div' $ strip xs) )

list_content :: [String] -> Bool
list_content [] = True
list_content xs =
    let closingTagIndex = (findTagIndex xs "/li" 0)
    in (list_element $ listSlice xs 0 closingTagIndex) && (list_content $ drop (closingTagIndex+1) xs)

list :: [String] -> Bool
list xs = ((head xs) == "ul") && ((last xs) == "/ul") && (list_content $ strip xs)

div' :: [String] -> Bool
div' xs = ((head xs) == "div") && ((last xs) == "/div") && (statement_list $ strip xs)

statement_list :: [String] -> Bool
statement_list [] = True
statement_list xs =
    let first_closer_index = findTagIndex xs ('/':(xs!!0)) 0
    in (statement $ listSlice xs 0 first_closer_index) && (statement_list $ drop (first_closer_index+1) xs)

statement :: [String] -> Bool
statement [] = True
statement xs = (terminal xs) || (list xs) || (div' xs)

title' :: [String] -> Bool
title' xs = ((head xs) == "title") && ((last xs) == "/title") && (terminal $ strip xs)

head' :: [String] -> Bool
head' xs = ((head xs) == "head") && ((last xs) == "/head") && (head_content $ strip xs)

head_content :: [String] -> Bool
head_content xs =
    let titleIndex = findTagIndex xs "title" 0
        titleCloseIndex = findTagIndex xs "/title" 0
    in  (statement_list $ listSlice xs 0 (titleIndex-1)) && (title' $ listSlice xs titleIndex titleCloseIndex) && (statement_list $ listSlice xs (titleCloseIndex+1) (length xs))

body' :: [String] -> Bool
body' xs = ((head xs) == "body") && ((last xs) == "/body") && (statement_list $ strip xs)

main' :: [String] -> Bool
main' xs =
    let headOpenIndex = findTagIndex xs "head" 0
        headCloseIndex = findTagIndex xs "/head" 0
        bodyOpenIndex = findTagIndex xs "body" 0
        bodyCloseIndex = findTagIndex xs "/body" 0
    
    in (statement_list $ listSlice xs 0 (headOpenIndex-1)) && (head' $ listSlice xs headOpenIndex headCloseIndex) && (statement_list $ listSlice xs (headCloseIndex+1) (bodyOpenIndex-1)) && (body' $ listSlice xs bodyOpenIndex bodyCloseIndex) && (statement_list $ listSlice xs (bodyCloseIndex+1) (length xs))

html :: [String] -> Bool
html xs = ((head xs) == "html") && ((last xs) == "/html") && (main' $ strip xs)

main = do
    handle <- openFile "file.html" ReadMode
    contents <- hGetContents handle
    let tokenStream = lex' [] "" False False contents
    print (html tokenStream)
    hClose handle