module Parse where

import Result (Result(..))
import ListOps (collect, splitOn)
import qualified Syntax

import Text.Read (readMaybe)

type ErrorValue = String


type Parser = Char -> ParseStack -> Result ErrorValue ParseStack

data Parsable = Expression Syntax.Expression | Quantifier Syntax.Quantifier deriving Show

data ParseFrame = ParseFrame {
                    parser :: Parser,
                    accumulator :: String,
                    value :: Maybe Parsable }

type ParseStack = [ParseFrame]


newParseFrame :: Parser -> ParseFrame
newParseFrame p = ParseFrame p "" Nothing

finishParse :: ParseStack -> Result ErrorValue ParseStack
finishParse _ = Error "Not Implemented"

parseQuantifier :: Parser
parseQuantifier _ [] = Error "Parsing a Quantifier Requires a Parse Frame"
parseQuantifier c (x:xs)
    | isSingleQuantifier c = parseSingleQuantifier c >>= finishQuantifierParse [c]
    | isEndOfRangedQuantifier c = let rValue = reverse $ accumulator x in
        parseRangedQuantifier rValue >>= finishQuantifierParse rValue
    | otherwise = Value $ (x { accumulator = c : accumulator x}) : xs where

    quantifierChars :: [Char]
    quantifierChars = ['?', '*', '+']

    isSingleQuantifier :: Char -> Bool
    isSingleQuantifier cx = cx `elem` quantifierChars

    isEndOfRangedQuantifier :: Char -> Bool
    isEndOfRangedQuantifier = (==) '}'

    updateQuantifier :: ParseFrame -> String -> Syntax.Quantifier -> ParseFrame
    updateQuantifier frame acc quan = frame { accumulator = acc, value = Just $ Quantifier quan }

    finishQuantifierParse :: String -> Syntax.Quantifier -> Result ErrorValue ParseStack
    finishQuantifierParse str = finishParse . (:xs) . updateQuantifier x str

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

