import Data.List

data Handsign = Rock | Scissors | Paper
     deriving (Eq, Show)

preyOf :: Handsign -> Handsign
preyOf Rock = Scissors
preyOf Paper = Rock
preyOf Scissors = Paper

winner :: [Handsign] -> Maybe Handsign
winner xs = case nub xs of
    [a, b] | preyOf a == b -> Just a
           | otherwise -> Just b
    otherwise -> Nothing

main = do
    print $ winner [Scissors, Rock]
    print $ winner [Rock, Scissors]
    print $ winner [Rock, Rock]
    print $ winner [Rock, Scissors, Paper]
    print $ winner [Rock, Scissors, Rock, Scissors]
