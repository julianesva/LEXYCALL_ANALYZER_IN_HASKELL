import Data.Char (isSpace, isDigit, isLetter, isAlpha, isAlphaNum)
import Data.List.Split (splitOn)
import Data.List.Utils (replace)
import Data.List (stripPrefix, isPrefixOf, isInfixOf,elemIndex)
import Data.Maybe (catMaybes)


-- Checks if a character can be part of a variable name
isVariableChar :: Char -> Bool
isVariableChar c = isAlphaNum c || c == '_'

-- Checks if a string is a valid variable name
isVariable :: String -> Bool
isVariable [] = False
isVariable (x:xs) = isAlpha x && all isVariableChar xs

-- Classifies a string into different types of tokens
classify :: String -> [(String, String)]
classify [] = []
classify (x:xs)
    | x == '*'  = ("*", "Multiplication Operator") : classify xs
    | x == '+'  = ("+", "Adittion Operator") : classify xs
    | x == '/'  = ("/", "Division Operator") : classify xs
    | x == '-'  = ("-", "Subtraction Operator") : classify xs
    | x == '('  = ("(", "Opening Parenthesis") : classify xs
    | x == ')'  = (")", "Closing Parenthesis") : classify xs
    | x == '^'  = ("^", "Power Operator") : classify xs
    | isDigit x = let (num, rest) = span (\c -> isDigit c || c == '.' || c == 'E' || c == 'e') (x:xs)
                  in (num, if '.' `elem` num || 'E' `elem` num || 'e' `elem` num then "Real" else "Integer") : classify rest
    | isLetter x = let (var, rest) = span (\c -> isLetter c || c == '_' || isDigit c) (x:xs)
                   in (var, if isVariable var then "Variable" else "Lexycal error(Variable name has a wrong composition)") : classify rest
    | otherwise = classify xs


-- Prints a tuple of token and its type. Checking if exist any type of lexycal error.
printTuple :: (String, String) -> IO ()
printTuple (token, typing)
  | dotAfterEorE token = putStrLn (token ++ " " ++ "Lexycal error()--------------------------FIX")
  | length (filter (== '.') token) > 1 = putStrLn (token ++ " " ++ "Lexycal error()--------------------------FIX")
  | otherwise = putStrLn (token ++ " " ++ typing)
  where
    dotAfterEorE :: String -> Bool
    dotAfterEorE str = case (elemIndex '.' str, elemIndex 'e' str, elemIndex 'E' str) of
                         (Just dotIndex, Just eIndex, _) -> dotIndex > eIndex
                         (Just dotIndex, _, Just eIndex) -> dotIndex > eIndex
                         _ -> False


-- Checks tuples for lexical errors
checkTuples :: [(String, String)] -> IO ()
checkTuples tuples = mapM_ checkTuple (zip [0..] tuples)
  where
    checkTuple (i, t@(x, y)) =
      if x == "-" 
         && (i == 0 || "Real" `isInfixOf` snd (tuples !! (i - 1)))
         && (i == length tuples - 1 || "Real" `isInfixOf` snd (tuples !! (i + 1)))
         && (i > 0 && ("e" `isInfixOf` fst (tuples !! (i - 1)) || "E" `isInfixOf` fst (tuples !! (i - 1))))
      then putStrLn (fst (tuples !! (i - 1)) ++ x ++ fst (tuples !! (i + 1)) ++ " " ++ "Lexycal error()--------------------------FIX")
      else putStrLn ""


-- Filters out spaces from a string
filterString :: String -> String
filterString = filter (not . isSpace)


-- Splits a string on delimiters
splitDelimiters :: String -> [String]
splitDelimiters "" = []
splitDelimiters s = case stripPrefix "=" s of
    Just rest -> "=" : splitDelimiters rest
    Nothing -> case stripPrefix "//" s of
        Just rest -> "//" : splitDelimiters rest
        Nothing -> let (token, rest) = break (`elem` ['=', '/']) s
                    in if token /= "" then token : splitDelimiters rest
                       else case rest of
                            ('/':rest') -> "/" : splitDelimiters rest'
                            _ -> splitDelimiters rest


-- Processes a line of input. Handling entire comment lines.
processLine :: String -> IO ()
processLine line = do
    if take 2 line == "//" then processLine_2 line
    else do
        let correct_line = filterString line
        processLine_2 correct_line

-- Processes a line of input.
processLine_2 :: String -> IO ()
processLine_2 line = if take 2 line == "//" then putStrLn (line ++ " Comment\n")
else do
   let string_parts = splitDelimiters line
   
   if not (null string_parts) && isVariable (head string_parts) then putStrLn (head string_parts ++ " Variable\n")
   else
       putStrLn (head string_parts ++ " Lexycal error(Variable name has a wrong composition)")

   if length string_parts > 1 && string_parts!!1 == "=" then putStrLn (string_parts!!1 ++ " Assignment Operator\n")
   else
       putStrLn (string_parts!!1 ++ "Lexycal error(Must be an assignmet operator)")

   if length string_parts > 2 then do
       let result_equa_1 = classify (string_parts!!2)
       mapM_ printTuple result_equa_1
       checkTuples result_equa_1
   else putStrLn ""

   if length string_parts > 3 && take 2 (string_parts!!3) == "//" then putStrLn (string_parts!!3 ++ (if length string_parts > 4 then string_parts!!4 else "") ++ " Comment\n")
   else if length string_parts > 3 && take 1 (string_parts!!3) == "/" then putStrLn (string_parts!!3 ++ " Division Operator\n")
   else putStrLn ""

   if length string_parts > 3 && not (take 2 (string_parts!!3) == "//") && length string_parts > 4 then do
       let result_equa_2 = classify (string_parts!!4)
       mapM_ printTuple result_equa_2
       checkTuples result_equa_2
    else putStrLn ""

   if length string_parts > 5 && take 2 (string_parts!!5) == "//"
   then putStrLn (string_parts!!5 ++ (if length string_parts > 6 then string_parts!!6 else "") ++ " Comment\n")
   else putStrLn ""



main :: IO ()
main = do
    content <- readFile "algebraic_expresions.txt"

    let linesOfText = lines content
    mapM_ processLine linesOfText

