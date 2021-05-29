import System.IO
import System.Exit

main = do
    handle <- openFile "file.html" ReadMode
    contents <- hGetContents handle
    evaluateContents contents
    hClose handle

evaluateContents :: String -> IO()
evaluateContents contents = do
    print (lex' [] "" contents)

lex' :: [String] -> String -> String -> [String]
lex' tokenStream currentTag (x:xs) = if validToken currentTag:x then lex' (tokenStream:(currentTag:x)) "" xs
                                    else ["Hello"]