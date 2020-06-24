module Hands where

import Cards
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)

getGroups hand =
  let equalRank x y = rank x == rank y
      groups = groupBy equalRank $ sortBy (comparing rank) hand
   in reverse $ sortBy (comparing length) groups

getSuit :: Card -> Suit
getSuit (Card r s) = s

allEq :: [Card] -> Maybe Card -> Bool
allEq (h:t) Nothing = allEq t (Just h)
allEq (h:t) (Just e) = (getSuit h == getSuit e) && allEq t (Just e)
allEq [] _ = True

isFlush :: [Card] -> Bool
isFlush hand = allEq hand Nothing

isStraight :: [Card] -> Bool
isStraight hands = False

getHighestCard :: [Card] -> Card
getHighestCard hand = last $ sortBy (comparing rank) hand

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


-- FIXME: This needs to return whether player wins or not.
-- playerWins :: [Card] -> [Card] -> Bool
-- playerWins playerHand aiHand = do
--     let playerRank = fst $ getHandValue playerHand
--     let aiRank = fst $ getHandValue aiHand
--     putStrLn playerRank
--     putStrLn aiRank
--     return True

bestHand :: [Card] -> String
bestHand hand =
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
