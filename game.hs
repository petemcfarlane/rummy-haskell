import Rummy
import System.Random

main :: IO ()
main = do
  (player, deck, discardedPile) <- setup
  play player deck discardedPile



setup :: IO (Player, Deck, Deck)
setup = do
  -- shuffle cards
  deck <- shuffleIO newDeck
  -- create player
  -- putStrLn "What is your name?"
  -- p1 <- newPlayer <$> getLine
  let player = newPlayer "Pete"
  -- deal 7 cards to all players
  let (deck', player') = dealNCardsToPlayer 7 deck player
  -- turn 1 card and make a discarded pile
  let (discardedCard, deck'') = deal deck'
  let discardedPile = [discardedCard]

  return (player', deck'', discardedPile)



play :: Player -> Deck -> Deck -> IO ()
play player deck discardedPile = do
  putStrLn $ "\n" ++ show player
  putStrLn $ "What would you like to do now?\n  1) take unknown card\n  2) take last discarded card (" ++ (show $ head discardedPile) ++ ")"
  answer <- readLn :: IO Int
  if answer == 1
    then takeUnknownCard player deck discardedPile
    else takeLastDiscardedCard player deck discardedPile



takeUnknownCard :: Player -> Deck -> Deck -> IO ()
takeUnknownCard player deck discardedPile = let (d4, p3) = dealCardToPlayer deck player
         in if canMeld (hand p3)
            then putStrLn $ "You have won! " ++ show (hand p3)
            else do putStrLn $ "\nYour hand: " ++ show (hand p3)
                    putStrLn "Choose a card to discard (1-8)"
                    a2 <- readLn :: IO Int
                    let (h, discardedPile3) = discard (hand p3) a2 discardedPile
                    play (Player (name player) h) d4 discardedPile3



takeLastDiscardedCard player deck discardedPile = let (discardedPile2, p3) = dealCardToPlayer discardedPile player
         in if canMeld (hand p3)
            then putStrLn $ "You have won! " ++ show (hand p3)
            else do putStrLn $ "\nYour hand: " ++ show (hand p3)
                    putStrLn "Choose a card to discard (1-8)"
                    a2 <- readLn :: IO Int
                    let (h, discardedPile3) = discard (hand p3) a2 discardedPile2
                    play (Player (name player) h) deck discardedPile3

