import System.IO
import System.Random

import Cards
import Hands

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          newDeck <- shuffleCards
          gameIntro
          playGame newDeck

gameIntro :: IO ()
gameIntro = do
        putStrLn "Welcome to Haskell poker"
        putStrLn "Select the card you want to discard by entering number from 1-5 and pressing enter"
        putStrLn "When you're ready just press enter"
        putStrLn "Good luck!"

playGame :: [Card] -> IO ()
playGame gameDeck = do
    putStrLn $ "Cards in deck: " ++ (show $ length gameDeck)
    deck <- if length gameDeck < 10
              then do
                   putStrLn ("Shuffling deck...")
                   newDeck <- shuffleCards
                   putStrLn ("Cards in deck: " ++ (show $ length newDeck))
                   return newDeck
              else return gameDeck

    (hand, deck) <- dealCards 5 deck
    hand <- discardCard hand hand
    (newCards, deck) <- dealCards (5 - length hand) deck
    putStrLn $ "You draw " ++ (show newCards)
    let newHand = hand ++ newCards
    putStrLn $ "Your hand is " ++ (show $ newHand)
    putStrLn $ "You have: " ++ (show $ bestHand newHand)
    putStr "Play again y/n? "
    char <- getLine
    if char == "y" then playGame deck else return ()

discardCard :: [Card] -> [Card] -> IO [Card]
discardCard originalHand hand = do
    let printHand  = show $ map (\card -> if elem card hand then card else BlankCard) originalHand
    putStr $ printHand ++ " - Discard card at position: "
    input <- getLine
    if length input == 0 then return hand else
        let discardAt           = read input
            cardToBeDiscarded   = originalHand !! (discardAt - 1)
            newHand             = filter (\card -> card /= cardToBeDiscarded) hand in
        discardCard originalHand newHand
