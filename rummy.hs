module Rummy where

import Data.List (sort, permutations, tails, (\\))
import System.Random
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef

data Suit =  Clubs | Diamonds | Hearts | Spades deriving (Eq, Ord, Enum)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
            | Jack | Queen | King | Ace deriving (Show, Eq, Ord, Enum)

data Card = Card Rank Suit deriving (Eq)
type Deck = [Card]
type Hand = [Card]
-- type PickupCard = Hand -> Card -> Hand
type Deal = Deck -> (Card, Deck)
data Player = Player { name :: String, hand :: Hand } deriving (Show)
type Discard = Hand -> Int -> Deck -> (Hand, Deck)
type GameState = (Player, Deck, Deck)

instance Show Card where
    show (Card Two suit)   = "2" ++ show suit
    show (Card Three suit) = "3" ++ show suit
    show (Card Four suit)  = "4" ++ show suit
    show (Card Five suit)  = "5" ++ show suit
    show (Card Six suit)   = "6" ++ show suit
    show (Card Seven suit) = "7" ++ show suit
    show (Card Eight suit) = "8" ++ show suit
    show (Card Nine suit)  = "9" ++ show suit
    show (Card Ten suit)   = "10" ++ show suit
    show (Card Jack suit)  = "J" ++ show suit
    show (Card Queen suit) = "Q" ++ show suit
    show (Card King suit)  = "K" ++ show suit
    show (Card Ace suit)   = "A" ++ show suit

instance Show Suit where
    show Spades = "♠"
    show Hearts = "♥"
    show Diamonds = "♦"
    show Clubs = "♣"

newDeck :: Deck
newDeck = [Card r s | s <- [Clubs .. ], r <- [Two .. ]]

newPlayer :: String -> Player
newPlayer name = Player name []

pete = newPlayer "Pete"

deal :: Deal
deal [] = error "Empty deck"
deal (x:xs) = (x, xs)

-- pickupCard :: PickupCard
-- pickupCard hand card = card : hand

dealCardToPlayer :: Deck -> Player -> (Deck, Player)
dealCardToPlayer [] _ = error "Empty deck"
dealCardToPlayer d (Player name hand) = let (card, d') = deal d
                                         in (d', Player name (card:hand))

dealNCardsToPlayer :: Int -> Deck -> Player -> (Deck, Player)
dealNCardsToPlayer n d p
    | n > length d = error "Not enough cards in deck"
    | n < 1        = error "Must deal at least once card"
    | n == 1       = dealCardToPlayer d p
    | otherwise    = dealNCardsToPlayer (n - 1) d' p'
        where (d', p') = dealCardToPlayer d p

discard :: Discard
discard h n d = let c = h !! (n - 1) in (h \\ [c], c:d)

showLastDiscarded :: GameState -> String
showLastDiscarded (_, _, discardPile) = show $ head discardPile

showHand :: GameState -> String
showHand ((Player _ hand), _, _) = show hand

cardsInGroup :: [Card] -> Bool
cardsInGroup [(Card r1 _), (Card r2 _), (Card r3 _)] = r1 == r2 && r1 == r3
cardsInGroup [(Card r1 _), (Card r2 _), (Card r3 _), (Card r4 _)] = r1 == r2 && r1 == r3 && r1 == r4
cardsInGroup _ = False

cardsInSequence :: [Card] -> Bool
cardsInSequence [(Card r1 s1), (Card r2 s2), (Card r3 s3)] = sameSuit && inSequence
    where sameSuit = s1 == s2 && s1 == s3
          inSequence = sorted !! 0 + 1 == sorted !! 1 && sorted !! 1 + 1 == sorted !! 2
          sorted = map fromEnum $ sort [r1, r2, r3]
cardsInSequence [(Card r1 s1), (Card r2 s2), (Card r3 s3), (Card r4 s4)] = sameSuit && inSequence
    where sameSuit = s1 == s2 && s1 == s3 && s1 == s4
          inSequence = sorted !! 0 + 1 == sorted !! 1 && sorted !! 1 + 1 == sorted !! 2 && sorted !! 2 + 1 == sorted !! 3
          sorted = map fromEnum $ sort [r1, r2, r3, r4]
cardsInSequence _ = False

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations n xs = [ y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

combinationsOfHand :: Hand -> [Hand]
combinationsOfHand = combinations 7

isWinning :: [Card] -> Bool
isWinning cards = cardsInSequence cards || cardsInGroup cards

canMeld :: Player -> Bool
canMeld (Player _ hand) = let combinationsOf7 = combinationsOfHand hand
                              winningCombinations hand' = [ (four, three)
                                                         | four <- combinations 4 hand'
                                                         , let three = hand' \\ four
                                                         , isWinning four
                                                         , isWinning three
                                                         ]
                              solutionExists  = \h -> (length $ winningCombinations h) >= 1
                          in  any solutionExists combinationsOf7

-- | Randomly shuffle a list without the IO Monad
--   /O(N)/
shuffle' :: [a] -> StdGen -> ([a],StdGen)
shuffle' xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n xs =  newListArray (1,n) xs

shuffleIO :: [a] -> IO [a]
shuffleIO xs = getStdRandom (shuffle' xs)

shuffleDeck :: Deck -> Int -> Deck
shuffleDeck deck seed = fst $ shuffle' deck $ mkStdGen seed

takeUnknownCard :: GameState -> GameState
takeUnknownCard (player, deck, discardedPile) = (player', deck', discardedPile)
                                                where (deck', player') = dealCardToPlayer deck player

takeLastDiscardedCard :: GameState -> GameState
takeLastDiscardedCard (player, deck, discardedPile) = (player', deck, discardedPile')
                                                      where (discardedPile', player') = dealCardToPlayer discardedPile player
