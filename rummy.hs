import Data.List (sort, permutations, tails, (\\))

data Suit =  Clubs | Diamonds | Hearts | Spades deriving (Eq, Ord, Enum)

instance Show Suit where
    show Spades = "♠"
    show Hearts = "♥"
    show Diamonds = "♦"
    show Clubs = "♣"

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
            | Jack | Queen | King | Ace deriving (Show, Eq, Ord, Enum)

data Card = Card Rank Suit deriving (Eq)

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

two :: Suit -> Card
two s = Card Two s

three :: Suit -> Card
three s = Card Three s

four :: Suit -> Card
four s = Card Four s

five :: Suit -> Card
five s = Card Five s

six :: Suit -> Card
six s = Card Six s

seven :: Suit -> Card
seven s = Card Seven s

eight :: Suit -> Card
eight s = Card Eight s

nine :: Suit -> Card
nine s = Card Nine s

ten :: Suit -> Card
ten s = Card Ten s

jack :: Suit -> Card
jack s = Card Jack s

queen :: Suit -> Card
queen s = Card Queen s

king :: Suit -> Card
king s = Card King s

ace :: Suit -> Card
ace s = Card Ace s

type Deck = [Card]

newDeck :: Deck
newDeck = [Card r s | s <- [Clubs .. Spades], r <- [Two .. Ace]]

shuffleDeck :: Deck -> Deck
shuffleDeck deck = undefined

type Hand = [Card]

data Player = Player { name :: String, hand :: Hand } deriving (Show)

newPlayer :: String -> Player
newPlayer name = Player name []

pete = newPlayer "Pete"

deal :: Deck -> (Card, Deck)
deal [] = error "Empty deck"
deal (x:xs) = (x, xs)

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

canMeld :: Hand -> Bool
canMeld hand = let combinationsOf7 = combinationsOfHand hand
                   winningCombinations hand' = [ (four, three)
                                               | four <- combinations 4 hand'
                                               , let three = hand' \\ four
                                               , isWinning four
                                               , isWinning three
                                               ]
                   solutionExists  = \h -> (length $ winningCombinations h) >= 1
                in any solutionExists combinationsOf7
