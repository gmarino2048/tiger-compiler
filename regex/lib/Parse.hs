module Parse where

import Result (Result(..))
import ListOps (collect, splitOn)
import qualified Syntax

import Text.Read (readMaybe)

type ErrorValue = String


type Parser = Char -> ParseStack -> Result ErrorValue ParseStack

data ParseFrame = ParseFrame {
                    parser :: Parser,
                    accumulator :: String,
                    expression :: Syntax.Expression }

type ParseStack = [ParseFrame]


data QuantifierParseState = QuantifierParseState {
                                sourceText :: String,
                                quantifier :: Maybe Syntax.Quantifier }
                                deriving Show


initQuantifier :: String -> Syntax.Quantifier -> Result e QuantifierParseState
initQuantifier s q = Value $ QuantifierParseState s $ Just q


parseQuantifier :: Char -> String -> Result ErrorValue QuantifierParseState
parseQuantifier '?' _     = initQuantifier "?" Syntax.OptionalQuantifier
parseQuantifier '*' _     = initQuantifier "*" Syntax.AnyCountQuantifier
parseQuantifier '+' _     = initQuantifier "+" Syntax.AtLeastOneQuantifier
parseQuantifier '}' str   = parseRangedQuantifier str
parseQuantifier other str = Value $ QuantifierParseState (other : str) Nothing


parseRangedQuantifier :: String -> Result ErrorValue QuantifierParseState
parseRangedQuantifier rangeValue = collectedResults >>= makeQuantifier where

    makeQuantifier :: [Maybe Integer] -> Result ErrorValue QuantifierParseState
    makeQuantifier [x, y] = case (x, y) of
        (Just a, Just b)  -> initQuantifier rangeValue $ Syntax.RangeQuantifier a b
        (Just a, Nothing) -> initQuantifier rangeValue $ Syntax.MinimumQuantifier a
        (Nothing, Just b) -> initQuantifier rangeValue $ Syntax.MaximumQuantifier b
        _                 -> Error "Ranged Quantifier requires at least one value"
    makeQuantifier _      = Error "Ranged Quantifier requires exactly two elements"

    collectedResults :: Result ErrorValue [Maybe Integer]
    collectedResults = collect $ map parseIntOrEmpty (splitOn rangeValue ',')

    parseIntOrEmpty :: String -> Result ErrorValue (Maybe Integer)
    parseIntOrEmpty [] = Value Nothing
    parseIntOrEmpty xs = case (readMaybe :: String -> Maybe Integer) xs of
        Just x  -> Value $ Just x
        Nothing -> Error $ "Failed to parse integer from \"" ++ xs ++ "\""

