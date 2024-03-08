module Parse where

import Result (Result(..))
import ListOps (collect, splitOn)
import qualified Syntax

import Text.Read (readMaybe)

type ErrorValue = String


type Parser = Char -> ParseStack -> Result ErrorValue ParseStack

data ParseFrame = ParseFrame {
                    parser :: Parser,
                    expression :: Syntax.Expression }

type ParseStack = [ParseFrame]


data QuantifierParseState = QuantifierParseState {
                                sourceText :: String,
                                quantifier :: Maybe Syntax.Quantifier }
                                deriving Show

parseQuantifier :: Char -> String -> Result ErrorValue QuantifierParseState
parseQuantifier '?' _     = Value $ QuantifierParseState "?" $ Just Syntax.OptionalQuantifier
parseQuantifier '*' _     = Value $ QuantifierParseState "*" $ Just Syntax.AnyCountQuantifier
parseQuantifier '+' _     = Value $ QuantifierParseState "+" $ Just Syntax.AtLeastOneQuantifier
parseQuantifier '}' str   = parseRangedQuantifier str
parseQuantifier other str = Value $ QuantifierParseState (other : str) Nothing


parseRangedQuantifier :: String -> Result ErrorValue QuantifierParseState
parseRangedQuantifier rangeValue = collectedResults >>= makeQuantifier where

    makeQuantifier :: [Maybe Integer] -> Result ErrorValue QuantifierParseState
    makeQuantifier [x, y] = case (x, y) of
        (Just a, Just b)  -> initQuantifier $ Syntax.RangeQuantifier a b
        (Just a, Nothing) -> initQuantifier $ Syntax.MinimumQuantifier a
        (Nothing, Just b) -> initQuantifier $ Syntax.MaximumQuantifier b
        _                 -> Error "Ranged Quantifier requires at least one value"
    makeQuantifier _      = Error "Ranged Quantifier requires two elements"

    initQuantifier :: Syntax.Quantifier -> Result e QuantifierParseState
    initQuantifier q = Value $ QuantifierParseState rangeValue $ Just q

    collectedResults :: Result ErrorValue [Maybe Integer]
    collectedResults = collect $ map parseIntOrEmpty (splitOn rangeValue ',')

    parseIntOrEmpty :: String -> Result ErrorValue (Maybe Integer)
    parseIntOrEmpty [] = Value Nothing
    parseIntOrEmpty xs = case (readMaybe :: String -> Maybe Integer) xs of
        Just x  -> Value $ Just x
        Nothing -> Error $ "Failed to parse integer from \"" ++ xs ++ "\""

