module Parse where

import Result (Result(..))
import ListOps (collect, splitOn)
import qualified Syntax

import Control.Monad (join)
import Text.Read (readMaybe)

type ErrorValue = String


type Parser = Char -> ParseStack -> Result ErrorValue ParseStack

data Parsable = Expression Syntax.Expression | Quantifier Syntax.Quantifier deriving Show

data ParseFrame = ParseFrame {
                    parser :: Parser,
                    accumulator :: String,
                    value :: Maybe Parsable }

type ParseStack = [ParseFrame]


--
-- This section contains support methods for the Parsers
--

newParseFrame :: Parser -> ParseFrame
newParseFrame p = ParseFrame p "" Nothing

updateParseFrame :: ParseFrame -> String -> Parsable -> ParseFrame
updateParseFrame frame acc parsed = frame { accumulator = acc, value = Just parsed }

popParseFrame :: ParseStack -> Result ErrorValue ParseStack
popParseFrame [] = Error "Parse Stack is empty, cannot pop frame from empty stack"
popParseFrame [_] = Error "Mismatched escape closure, cannot return from final context"
popParseFrame (x:y:ys) = let updatedExpression = join (modifyExpression <$> valueFrom x <*> expressionFrom y) in
    (:ys) . modifyFrame y <$> updatedExpression where

    valueFrom :: ParseFrame -> Result ErrorValue Parsable
    valueFrom frame = case value frame of
        Just v -> Value v
        _      -> Error "Parse Frame does not have a value"

    expressionFrom :: ParseFrame -> Result ErrorValue Syntax.Expression
    expressionFrom frame = valueFrom frame >>= getExpression where

        getExpression :: Parsable -> Result ErrorValue Syntax.Expression
        getExpression (Expression e) = Value e
        getExpression _ = Error "Expected expression from parse frame"

    modifyFrame :: ParseFrame -> Syntax.Expression -> ParseFrame
    modifyFrame frame expr = frame { value = Just $ Expression expr }

    modifyExpression :: Parsable -> Syntax.Expression -> Result ErrorValue Syntax.Expression
    modifyExpression _ _ = Error "Not Implemented"


--
-- This section contains the quantifier parser
--

parseQuantifier :: Parser
parseQuantifier _ [] = Error "Parsing a Quantifier Requires a Parse Frame"
parseQuantifier c (x:xs)
    | isSingleQuantifier c = parseSingleQuantifier c >>= finishQuantifierParse [c]
    | isEndOfRangedQuantifier c = let rValue = reverse $ accumulator x in
        parseRangedQuantifier rValue >>= finishQuantifierParse rValue
    | otherwise = Value $ (x { accumulator = c : accumulator x }) : xs where

    quantifierChars :: [Char]
    quantifierChars = ['?', '*', '+']

    isSingleQuantifier :: Char -> Bool
    isSingleQuantifier cx = cx `elem` quantifierChars

    isEndOfRangedQuantifier :: Char -> Bool
    isEndOfRangedQuantifier = (==) '}'

    finishQuantifierParse :: String -> Syntax.Quantifier -> Result ErrorValue ParseStack
    finishQuantifierParse str = popParseFrame . (:xs) . updateParseFrame x str . Quantifier

parseSingleQuantifier :: Char -> Result ErrorValue Syntax.Quantifier
parseSingleQuantifier '?' = Value Syntax.OptionalQuantifier
parseSingleQuantifier '*' = Value Syntax.AnyCountQuantifier
parseSingleQuantifier '+' = Value Syntax.AtLeastOneQuantifier
parseSingleQuantifier cx  = Error $ "Invalid Single Quantifier: '" ++ [cx] ++ "'"

parseRangedQuantifier :: String -> Result ErrorValue Syntax.Quantifier
parseRangedQuantifier rangeValue = collectedResults >>= makeQuantifier where

    makeQuantifier :: [Maybe Integer] -> Result ErrorValue Syntax.Quantifier
    makeQuantifier [x, y] = case (x, y) of
        (Just a, Just b)  -> Value $ Syntax.RangeQuantifier a b
        (Just a, Nothing) -> Value $ Syntax.MinimumQuantifier a
        (Nothing, Just b) -> Value $ Syntax.MaximumQuantifier b
        _                 -> Error "Ranged Quantifier requires at least one value"
    makeQuantifier _      = Error "Ranged Quantifier requires exactly two elements"

    collectedResults :: Result ErrorValue [Maybe Integer]
    collectedResults = collect $ map parseIntOrEmpty (splitOn rangeValue ',')

    parseIntOrEmpty :: String -> Result ErrorValue (Maybe Integer)
    parseIntOrEmpty [] = Value Nothing
    parseIntOrEmpty xs = case (readMaybe :: String -> Maybe Integer) xs of
        Just x  -> Value $ Just x
        Nothing -> Error $ "Failed to parse integer from \"" ++ xs ++ "\""


--
-- This section contains the MatchGroup parser
--

parseMatchGroup :: Parser
parseMatchGroup _ [] = Error "Parsing a match group requires a Parse Frame"
parseMatchGroup c (x:xs)
    | isEndOfMatchGroup c = let rValue = reverse $ accumulator x in
        parseAccumulatedMatchGroup rValue >>= finishMatchGroupParse rValue
    | otherwise = Value $ (x { accumulator = c : accumulator x }) : xs where

    isEndOfMatchGroup :: Char -> Bool
    isEndOfMatchGroup = (==) ']'

    finishMatchGroupParse :: String -> Syntax.Expression -> Result ErrorValue ParseStack
    finishMatchGroupParse str = popParseFrame . (:xs) . updateParseFrame x str . Expression


parseAccumulatedMatchGroup :: String -> Result ErrorValue Syntax.Expression
parseAccumulatedMatchGroup _ = Error "Not Implemented"
