{-# LANGUAGE InstanceSigs #-}

module Result (Result(..)) where

data Result e v = Error e | Value v

instance Functor (Result e) where
    fmap:: (a -> b) -> Result e a -> Result e b
    fmap f (Value x) = Value $ f x
    fmap _ (Error e) = Error e


instance Applicative (Result e) where
    pure :: v -> Result e v
    pure = Value

    (<*>) :: Result e (a -> b) -> Result e a -> Result e b
    (<*>) (Value f) (Value x) = Value $ f x
    (<*>) (Error e) _         = Error e
    (<*>) _         (Error e) = Error e


instance Monad (Result e) where
    (>>=) :: Result e a -> (a -> Result e b) -> Result e b
    (>>=) (Value x) f = f x
    (>>=) (Error e) _ = Error e

