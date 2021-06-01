import System.IO

listSlice :: [a] -> Int -> Int -> [a]
listSlice xs i j = take (j-i+1) (drop i xs)

findTagIndex :: [String] -> String -> Int -> Int
findTagIndex [] tag n = error $ "Expected tag " ++ ('<':tag++['>'])
findTagIndex (x:xs) tag n = if x==tag then n else findTagIndex xs tag (n+1)

findCloserTag :: [String] -> String -> Int -> Int -> Int
findCloserTag [] tag depth accumulator = error $ "Error with tag " ++ tag
findCloserTag (x:xs) tag depth accumulator =
    if x==('/':tag) then 
        if depth==0 then
            accumulator
        else findCloserTag xs tag (depth-1) (accumulator+1)
    else if x==tag then
        findCloserTag xs tag (depth+1) (accumulator+1)
    else findCloserTag xs tag depth (accumulator+1)

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

terminal :: [String] -> Bool
terminal [] = True
terminal ["br"] = True
terminal ["hr"] = True
terminal ["h1", "/h1"] = True
terminal ["h2", "/h2"] = True
terminal ["h3", "/h3"] = True
terminal ["a", "/a"] = True
terminal xs = False

list_element :: [String] -> Bool
list_element xs = ((head xs) == "li") && ((last xs) == "/li") && ( (terminal $ strip xs) || (div' $ strip xs) || (p' $ strip xs) )

list_content :: [String] -> Bool
list_content [] = True
list_content xs =
    let closingTagIndex = (findTagIndex xs "/li" 0)
    in (list_element $ listSlice xs 0 closingTagIndex) && (list_content $ drop (closingTagIndex+1) xs)

list :: [String] -> Bool
list xs = ((head xs) == "ul") && ((last xs) == "/ul") && (list_content $ strip xs)

div' :: [String] -> Bool
div' xs = ((head xs) == "div") && ((last xs) == "/div") && (statement_list $ strip xs)

p' :: [String] -> Bool
p' xs = ((head xs) == "p") && ((last xs) == "/p") && (terminal $ strip xs)

statement_list :: [String] -> Bool
statement_list [] = True
statement_list ("br":xs) = statement_list xs
statement_list ("hr":xs) = statement_list xs
statement_list xs =
    let first_closer_index = findCloserTag xs (xs!!0) (-1) 0
    in (statement $ listSlice xs 0 first_closer_index) && (statement_list $ drop (first_closer_index+1) xs)

statement :: [String] -> Bool
statement [] = True
statement xs = 
    if (terminal xs) || (list xs) || (div' xs) || (p' xs) then
        True
    else
        let tokens = map (\x -> ('<':x)++['>']) xs
        in error $ "Code snippet\n---\n" ++ (unwords tokens) ++ "\n---\n is not a valid statement."

title' :: [String] -> Bool
title' xs =
    if ((head xs) == "title") && ((last xs) == "/title") then
        if (terminal $ strip xs) then
            True --Not ideal to have a "if (x) then True" statement, but must use for error messages.
        else
            let tokens = map (\x -> ('<':x)++['>']) xs
            in error $ "Title should only contain non-flow elements. Code snippet\n---\n" ++ (unwords tokens) ++ "\n---\n contains flow elements, such as <div> or <ul>"
    else
        error "Title section should be wrapped in <title> tags"

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
html xs =
    if ((head xs) == "html") && ((last xs) == "/html") then
        main' $ strip xs
    else
        error "File must be wrapped in <html> tags"

main = do
    handle <- openFile "file.html" ReadMode
    contents <- hGetContents handle
    let tokenStream = lex' [] "" False False contents
    if (html tokenStream) then print "Parsing successful" else print "Parsing failed"
    hClose handle