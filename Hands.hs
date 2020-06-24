module Hands where

import Data.List ( groupBy, sortBy )
import Data.Ord ( comparing )

import Cards

getGroups hand =
    let equalRank = (\x y -> rank x == rank y)
        groups = groupBy equalRank $ sortBy (comparing rank) hand in
        reverse $ sortBy (comparing length) groups

bestHand :: [Card] -> Maybe (String, [[Card]])
bestHand hand = rankMatches hand

rankMatches :: [Card] -> Maybe (String, [[Card]])
rankMatches hand =
    let longest = getGroups hand !! 0
        secondLongest = getGroups hand !! 1
    in
    case length longest of
        4 -> Just ("Four of a kind", [longest])
        3 -> case length secondLongest of
            2 -> Just ("Full house", [longest, secondLongest])
            _ -> Just ("Three of a kind", [longest])
        2 -> case length secondLongest of
            2 -> Just ("Two pair", [longest, secondLongest])
            _ -> Just ("Two of a kind", [longest])
        _ -> Nothing
