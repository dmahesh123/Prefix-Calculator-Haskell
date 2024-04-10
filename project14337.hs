--Dharshini Mahesh Project 1 4337.502 Salazar

import Data.Char
import Data.Maybe

-- Define the data types for history entries and expressions
data Expr = Val Int | Add Expr Expr | Multiply Expr Expr | Divide Expr Expr | Negate Expr | HistoryReference Int deriving Show
data HistoryEntry = HistoryEntry { entryID :: Int, result :: Int, expression :: String } deriving Show
type History = [HistoryEntry]

-- Function that evaluates prefix expression given by the user
evaluatePrefix :: String -> History -> Maybe (Int, History)
evaluatePrefix expr history = case parseExpressionHelper expr of
    Just (x, []) -> case evaluateExpression x history of
        Just res -> Just (res, addEntryToHistory expr res history)
        Nothing -> Nothing
    _ -> Nothing

-- Function that parses the expression into an expression tree
parseExpressionHelper :: String -> Maybe (Expr, String)
parseExpressionHelper [] = Nothing -- empty input
parseExpressionHelper ('(':rest) = parseParentheses rest
parseExpressionHelper ('+':rest) = parseBinaryOperator Add rest
parseExpressionHelper ('*':rest) = parseBinaryOperator Multiply rest
parseExpressionHelper ('/':rest) = parseBinaryOperator Divide rest
parseExpressionHelper ('-':rest) = parseUnaryOperator Negate rest
parseExpressionHelper ('$':rest) = parseHistory rest
parseExpressionHelper expr@(c:cs)
    | isDigit c = parseValue expr
    | isSpace c = parseExpressionHelper cs
    | otherwise = Nothing -- Invalid inputs

-- Function that parses a parenthesis expression
parseParentheses :: String -> Maybe (Expr, String)
parseParentheses expr = do
    (x, rest) <- parseExpressionHelper expr
    case dropWhile isSpace rest of
        ')':remaining -> Just (x, remaining)
        _ -> Nothing
    
-- Function that parses numeric values in prefix expression
parseValue :: String -> Maybe (Expr, String)
parseValue expr = case span isDigit expr of
    (num, rest) -> Just (Val (read num), rest)
    
-- Function that parses binary operators
parseBinaryOperator :: (Expr -> Expr -> Expr) -> String -> Maybe (Expr, String)
parseBinaryOperator op rest = do
    (x1, rest1) <- parseExpressionHelper rest
    (x2, rest2) <- parseExpressionHelper rest1
    return (op x1 x2, rest2)

-- Function that parses unary operator
parseUnaryOperator :: (Expr -> Expr) -> String -> Maybe (Expr, String)
parseUnaryOperator op rest = do
    (x, rest') <- parseExpressionHelper rest
    return (op x, rest')

-- Function that parses a HistoryReference
parseHistory :: String -> Maybe (Expr, String)
parseHistory expr = case span isDigit expr of
    (num, rest) -> if null num
                      then Nothing
                      else Just (HistoryReference (read num), rest)
-- Function to check if the user has input remaining Char after parsing
checkStringEnd :: (Expr, String) -> Maybe (Expr, String)
checkStringEnd (e, []) = Just (e, [])
checkStringEnd (_, rest) = if all isSpace rest then Just (Val 0, []) else Nothing

-- Function that evaluates the expression tree
evaluateExpression :: Expr -> History -> Maybe Int
evaluateExpression (Val n) _ = Just n
evaluateExpression (Add x1 x2) history = (+) <$> evaluateExpression x1 history <*> evaluateExpression x2 history
evaluateExpression (Multiply x1 x2) history = (*) <$> evaluateExpression x1 history <*> evaluateExpression x2 history
evaluateExpression (Divide x1 x2) history = do
    x1Value <- evaluateExpression x1 history
    x2Value <- evaluateExpression x2 history
    if x2Value /= 0
        then Just (x1Value `div` x2Value)
        else Nothing --divide by 0
evaluateExpression (Negate x) history = negate <$> evaluateExpression x history
evaluateExpression (HistoryReference n) history = result <$> findEntryInHistory n history

-- Function that finds the entry history through the ID
findEntryInHistory :: Int -> History -> Maybe HistoryEntry
findEntryInHistory n history = case reverse history !? (n - 1) of
    Just entry -> Just entry
    Nothing -> Nothing
    
-- Function that adds a entry by ID to history
addEntryToHistory :: String -> Int -> History -> History
addEntryToHistory expr result history =
    HistoryEntry { entryID = length history + 1, result = result, expression = expr } : history
    
-- Custom operators to safely index through the list
(!?) :: [a] -> Int -> Maybe a
[] !? _ = Nothing
(y:_) !? 0 = Just y
(_:ys) !? n = ys !? (n - 1)

-- Main
main :: IO ()
main = do
    putStrLn "Prefix Notation Expression Calculator!"
    loop []
    
-- Continous loop
loop :: History -> IO ()
loop history = loop' history
    where
        loop' :: History -> IO ()
        loop' hist = do
        putStrLn "Enter the Prefix Expression to Evaluate: "
        input <- getLine
        case input of
            "exit" -> putStrLn "Exiting the Program"
            _ -> case evaluatePrefix input hist of
                Just (answer, newHistory) -> do
                    putStrLn $ "Result (ID " ++ show (length newHistory) ++ "): " ++ show answer
                    loop' newHistory
                Nothing -> do
                    putStrLn "Invalid Expression"
                    loop' hist -- loop back 

