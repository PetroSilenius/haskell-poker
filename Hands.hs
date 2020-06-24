module Hands where

import Cards
import Data.List (groupBy, sortOn)

getGroups hand =
  let equalRank x y = rank x == rank y
      groups = groupBy equalRank $ sortOn rank hand
   in reverse $ sortOn length groups

getSuit :: Card -> Suit
getSuit (Card r s) = s

getRank :: Card -> Rank
getRank (Card r s) = r

getIntRank :: Card -> Int
getIntRank (Card r s) =
    case r of
        Two -> 2
        Three -> 3
        Four -> 4
        Five -> 5
        Six -> 6
        Seven -> 7
        Eight -> 8
        Nine -> 9
        Ten -> 10
        Jack -> 11
        Queen -> 12
        King -> 13
        Ace -> 14

allEq :: [Card] -> Maybe Card -> Bool
allEq (h:t) Nothing = allEq t (Just h)
allEq (h:t) (Just e) = (getSuit h == getSuit e) && allEq t (Just e)
allEq [] _ = True

isFlush :: [Card] -> Bool
isFlush hand = allEq hand Nothing

isStraight :: [Card] -> Bool
isStraight hand =
    case map getRank $ sortOn rank hand of
        [Two, Three, Four, Five, Ace] -> True
        [Two, Three, Four, Five, Six] -> True
        [Three, Four, Five, Six, Seven] -> True
        [Four, Five, Six, Seven, Eight] -> True
        [Five, Six, Seven, Eight, Nine] -> True
        [Six, Seven, Eight, Nine, Ten] -> True
        [Seven, Eight, Nine, Ten, Jack] -> True
        [Eight, Nine, Ten, Jack, Queen] -> True
        [Nine, Ten, Jack, Queen, King] -> True
        [Ten, Jack, Queen, King, Ace] -> True
        _ -> False

getHighestCard :: [Card] -> Card
getHighestCard hand = last $ sortOn rank hand

getHandValue :: [Card] -> (Int, Card)
getHandValue hand =
  let longest = head (getGroups hand)
      secondLongest = getGroups hand !! 1
  in case length longest of
      4 -> (8, head longest)
      3 -> case length secondLongest of
          2 -> (7, head longest)
          _ -> (4, head longest)
      2 -> case length secondLongest of
          2 -> (3, head longest)
          _ -> (2, head longest)
      _ ->
          if isFlush hand && isStraight hand then
              (9, getHighestCard hand)
          else if isFlush hand then
              (6, getHighestCard hand)
          else (if isStraight hand then 5 else 1, getHighestCard hand)

getHandText :: [Card] -> String
getHandText hand =
  case getHandValue hand of
    (9, highCard) -> "Straight flush"
    (8, highCard) -> "Four of a kind"
    (7, highCard) -> "Full house"
    (6, highCard) -> show (getRank highCard) ++ " high flush"
    (5, highCard) -> show (getRank highCard) ++ " high straigh"
    (4, highCard) -> "Three of a kind"
    (3, highCard) -> "Two of a kind"
    (2, highCard) -> "Pair of " ++ show (getRank highCard) ++ "s"
    (1, highCard) -> show (getRank highCard) ++ " high"

playerWins :: [Card] -> [Card] -> Maybe Bool
playerWins playerHand aiHand = do
    let playerRank = getHandValue playerHand
    let aiRank = getHandValue aiHand
    let playerValue = 100 * fst playerRank + getIntRank (snd playerRank)
    let aiValue = 100 * fst aiRank + getIntRank (snd aiRank)
    if playerValue > aiValue then
        Just True
    else
        if playerValue < aiValue then Just False else Nothing
