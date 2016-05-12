{-# LANGUAGE NamedFieldPuns #-}

{-|

Data structure and associated operations for working with flag parameters.

|-}
module Data.Flag where

data Bound = MustNot| Must deriving (Eq, Ord)
data Flag = Fl { lb, ub :: Bound } deriving Eq

instance Show Bound where
  show Must    = "+"
  show MustNot = "-"

instance Show Flag where
  show Fl {lb, ub}
    | lb < ub   = "[~]"
    | lb > ub   = "[x]"
    | otherwise = "[" ++ show lb ++ "]"

must, mustNot, dontCare, inconsistent :: Flag
must         = Fl Must Must
mustNot      = Fl MustNot MustNot
dontCare     = Fl MustNot Must
inconsistent = Fl Must MustNot

(\/), (/\) :: Flag -> Flag -> Flag

(Fl lb ub) \/ (Fl lb' ub') = Fl (lb `min` lb') (ub `max` ub')
(Fl lb ub) /\ (Fl lb' ub') = Fl (lb `max` lb') (ub `min` ub')
