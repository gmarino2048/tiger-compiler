module Parse where

import Result (Result(..))
import qualified Syntax

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
parseRangedQuantifier revValue = let value = reverse revValue in

