module Cards where

import Data.List ( sortBy )
import Data.Ord ( comparing )
import System.Random

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord, Enum)

instance Show Rank where
    show Two = "2"
    show Three = "3"
    show Four = "4"
    show Five = "5"
    show Six = "6"
    show Seven = "7"
    show Eight = "8"
    show Nine = "9"
    show Ten = "10"
    show Jack = "J"
    show Queen = "Q"
    show King = "K"
    show Ace = "A"

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show, Eq)
data Card = Card { rank :: Rank, suit :: Suit } | BlankCard deriving (Eq)

instance Show Card where
    show BlankCard = "--"
    show card = show (rank card) ++ [head $ show $ suit card]

-- Construct card deck with nested loops of suits and ranks. --
getAllCards :: [Card]
getAllCards = [Card rank suit | suit <- [Hearts, Diamonds, Clubs, Spades], rank <- [Two .. Ace]]

-- Get shuffled deck with a random number. --
shuffleCards :: IO [Card]
shuffleCards = do gen <- getStdGen
                  let rnd = randoms gen :: [Integer]
                  return (map snd $ sortBy (comparing fst) $ zip rnd getAllCards)

-- Deal n cards from deck. --
dealCards :: Int -> [Card] -> IO ([Card], [Card])
dealCards n deck = do return (splitAt n deck)
