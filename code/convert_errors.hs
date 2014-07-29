-- convert_errors.hs
-- use in a custom build step like this: ghc MyFile.hs -o MyFile.exe 2>&1 | convert_errors.exe
-- the 2>&1 thing converts stderr to stdout

import System.IO
import System.Exit

breakDroppingMatchedChar predicate list = (leftSide, rightSide)
    where
    (leftSide, breakResult2) = break predicate list
    fixRightSide [] = error "Can't parse error statement"
    fixRightSide (x:xs) = xs
    rightSide = fixRightSide breakResult2

replaceChar (search,replace) s = leftSide ++ [replace] ++ rightSide
    where (leftSide, rightSide) = breakDroppingMatchedChar (== search) s

-- convert :10:20: into visual studio error marker style like (10,20)
convertColons input = foldr replaceChar input replaceList
replaceList = reverse [ ( ':', '(' ) , ( ':', ',' ) , ( ':', ')' ) ]

convertAfterPath (a,[]) = convertColons a
convertAfterPath (a,b) = a ++ convertColons b

-- must go passed the first colon in the path
convertErrorFormat inputLine = convertAfterPath (break ('\\'==) inputLine)

-- lines that start with a space aren't file locations for errors
parse_line [] = [] 
parse_line (x:xs) | x == ' '    = (x:xs)
                  | x == 'L'    = (x:xs)
                  | x == '['    = (x:xs)
                  | otherwise   = convertErrorFormat (x:xs) 

processStream = unlines . map parse_line . lines

line_has_error [] = False
line_has_error (x:xs) | x == ' '    = False
                      | x == 'L'    = False
                      | x == '['    = False
                      | otherwise   = True

has_error inputStream = any (True==) (map line_has_error (lines inputStream))

main =
    do
        inputStream <- getContents
        putStrLn (processStream inputStream)

        -- must error to tell visual studio build to stop
        if (has_error inputStream) 
            then putStrLn "Compile Failed" >> exitFailure
            else putStrLn "Compile success"
