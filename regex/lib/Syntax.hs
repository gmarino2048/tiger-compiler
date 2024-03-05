module Syntax (
    Match(..),
    Quantifier(..),
    Expression(..)
) where

data Match = AnyMatch
           | SingleMatch Char
           | RangeMatch Char Char
           | GroupMatch [Match]
           deriving Show


data Quantifier = SingleQuantifier
                | OptionalQuantifier
                | AnyCountQuantifier
                | AtLeastOneQuantifier
                | MinimumQuantifier Integer
                | MaximumQuantifier Integer
                | RangeQuantifier Integer Integer
                deriving Show


data Expression = MatchExpression {
                    match :: Match,
                    quantifier :: Quantifier,
                    sourceText :: String }
                | SubExpression {
                    expressionList :: [Expression],
                    instanceNumber :: Integer,
                    quantifier :: Quantifier,
                    sourceText :: String }
                deriving Show

