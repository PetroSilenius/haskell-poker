import System.IO
import System.Random
import Data.List.Split

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
        putStrLn ""

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

    (aiHand, deck) <- dealCards 5 deck
    let aiInitialHand = aiHand
    aiHand <- aiDiscardCard aiHand aiHand
    (aiNewCards, deck) <- dealCards (5 - length aiHand) deck
    let aiNewHand = aiHand ++ aiNewCards

    (hand, deck) <- dealCards 5 deck
    hand <- discardCard hand hand
    (newCards, deck) <- dealCards (5 - length hand) deck
    let newHand = hand ++ newCards

    putStrLn ""
    putStrLn $ "You draw " ++ (show newCards)
    putStrLn $ "Your hand is " ++ (show $ newHand)
    putStrLn $ "You have: " ++ (show $ getHandText newHand)
    putStrLn ""

    putStrLn $ "AI's initial hand was " ++ (show $ aiInitialHand)
    putStrLn $ "AI's hand at the end was " ++ (show $ aiNewHand)
    putStrLn $ "Ai has: " ++ (show $ getHandText aiNewHand)
    putStrLn ""

    let resultsFile = "results.txt"
    results <- readFile resultsFile
    let parsedResults = splitOn "," results
    let wins = read $ head parsedResults :: Integer
    let losses = read $ parsedResults !! 1 :: Integer
    -- playerWins $ newHand aiNewHand
    -- TODO: Increment wins/losses based on `playerWins` result.
    putStrLn $ ("Wins: " ++ show wins ++ ", Losses: " ++ show losses)
    writeFile resultsFile (show wins ++ "," ++ show losses)
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

aiDiscardCard :: [Card] -> [Card] -> IO [Card]
aiDiscardCard originalHand hand = do
    discardAt <- randomRIO (1,5)
    if length hand < 4 then return hand else
        let cardToBeDiscarded   = originalHand !! (discardAt - 1)
            newHand             = filter (\card -> card /= cardToBeDiscarded) hand in
        aiDiscardCard originalHand newHand
