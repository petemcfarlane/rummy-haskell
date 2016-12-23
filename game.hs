import Rummy
import System.Random

main :: IO ()
main = do
  initialState <- setup
  play initialState



setup :: IO (GameState)
setup = do
  -- shuffle cards
  deck <- shuffleIO newDeck
  -- create player
  putStrLn "What is your name?"
  player <- newPlayer <$> getLine
  -- let player = newPlayer "Pete"
  -- deal 7 cards to all players
  let (deck', player') = dealNCardsToPlayer 7 deck player
  -- turn 1 card and make a discarded pile
  let (discardedCard, deck'') = deal deck'
  let discardedPile = [discardedCard]

  return (player', deck'', discardedPile)



play :: GameState -> IO ()
play state@(player, _, _) =
  if canMeld player
  then putStrLn $ "You have won! " ++ showHand state
  else do
    state' <- pickupCard state
    state'' <- discardCard state'
    play state''


pickupCard :: GameState -> IO GameState
pickupCard state = do
  putStrLn $ "\nYour hand: " ++ showHand state
  putStrLn $ "What would you like to do now?\n  1) take unknown card\n  2) take last discarded card (" ++ (showLastDiscarded state) ++ ")"
  answer <- readLn
  return $ (if answer == 1 then takeUnknownCard state else takeLastDiscardedCard state)


discardCard :: GameState -> IO GameState
discardCard ((Player name hand), remaining, discardedPile) = do
  putStrLn $ "\nYour hand: " ++ show hand
  putStrLn "Choose a card to discard (1-8)"
  n <- readLn
  let (hand', discardedPile') = discard hand n discardedPile
  return ((Player name hand'), remaining, discardedPile')

