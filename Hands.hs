module Hands where

import Cards
import Data.List (groupBy, sortOn)
import Data.Ord (comparing)

getGroups hand =
  let equalRank x y = rank x == rank y
      groups = groupBy equalRank $ sortOn rank hand
   in reverse $ sortOn length groups

getSuit :: Card -> Suit
getSuit (Card r s) = s

getRank :: Card -> Rank
getRank (Card r s) = r

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
          2 -> (7, head secondLongest)
          _ -> (4, head secondLongest)
      2 -> case length secondLongest of
          2 -> (3, head secondLongest)
          _ -> (2, head secondLongest)
      _ ->
          if isFlush hand && isStraight hand
            then (9, getHighestCard hand)
            else
              if isFlush hand
                then (6, getHighestCard hand)
                else (if isStraight hand then 5 else 1, getHighestCard hand)

getHandText :: [Card] -> String
getHandText hand =
  case getHandValue hand of
    (9, _) -> "Straight flush"
    (8, _) -> "Four of a kind"
    (7, _) -> "Full house"
    (6, _) -> "Flush"
    (5, _) -> "Straight"
    (4, _) -> "Three of a kind"
    (3, _) -> "Two of a kind"
    (2, _) -> "Pair"
    (1, _) -> "High card"

-- FIXME: This needs to return whether player wins or not.
-- playerWins :: [Card] -> [Card] -> Bool
-- playerWins playerHand aiHand = do
--     let playerRank = fst $ getHandValue playerHand
--     let aiRank = fst $ getHandValue aiHand
--     putStrLn playerRank
--     putStrLn aiRank
--     return True
