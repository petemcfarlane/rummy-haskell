data Suit =  Clubs | Diamonds | Hearts | Spades deriving (Eq, Ord, Enum)

instance Show Suit where
    show Spades = "♠"
    show Hearts = "♥"
    show Diamonds = "♦"
    show Clubs = "♣"

data Card = Two Suit | Three Suit | Four Suit | Five Suit | Six Suit | Seven Suit | Eight Suit
            | Nine Suit | Ten Suit | Jack Suit | Queen Suit | King Suit | Ace Suit deriving (Eq, Ord)

instance Show Card where
    show (Ace suit) = "A" ++ show suit
    show (King suit) = "K" ++ show suit
    show (Queen suit) = "Q" ++ show suit
    show (Jack suit) = "J" ++ show suit
    show (Ten suit) = "10" ++ show suit
    show (Nine suit) = "9" ++ show suit
    show (Eight suit) = "8" ++ show suit
    show (Seven suit) = "7" ++ show suit
    show (Six suit) = "6" ++ show suit
    show (Five suit) = "5" ++ show suit
    show (Four suit) = "4" ++ show suit
    show (Three suit) = "3" ++ show suit
    show (Two suit) = "2" ++ show suit

type Deck = [Card]

newDeck :: Deck
newDeck = [r s | s <- [Clubs .. Spades], r <- [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]]

shuffleDeck :: Deck -> Deck
shuffleDeck deck = undefined

type Hand = [Card]

data Player = Player { name :: String, hand :: Hand } deriving (Show)

newPlayer :: String -> Player
newPlayer name = Player name []

pete = newPlayer "Pete"

deal :: Deck -> (Card, Deck)
deal [] = error "Empty deck"
deal (h:t) = (h, t)

dealCardToPlayer :: Deck -> Player -> (Deck, Player)
dealCardToPlayer [] _ = error "Empty deck"
dealCardToPlayer d (Player name hand) = let (card, newDeck) = deal d
                    in (newDeck, Player name (card : hand))

dealNCardsToPlayer :: Int -> Deck -> Player -> (Deck, Player)
dealNCardsToPlayer n d p
    | n > length d = error "Not enough cards in deck"
    | n < 1        = error "Must deal at least once card"
    | n == 1       = dealCardToPlayer d p
    | otherwise    = dealNCardsToPlayer (n - 1) d' p'
        where (d', p') = dealCardToPlayer d p

cardsInGroup :: [Card] -> Bool
cardsInGroup [Two _, Two _, Two _] = True
cardsInGroup [Two _, Two _, Two _, Two _] = True
cardsInGroup [Three _, Three _, Three _] = True
cardsInGroup [Three _, Three _, Three _, Three _] = True
cardsInGroup [Four _, Four _, Four _] = True
cardsInGroup [Four _, Four _, Four _, Four _] = True
cardsInGroup [Five _, Five _, Five _] = True
cardsInGroup [Five _, Five _, Five _, Five _] = True
cardsInGroup [Six _, Six _, Six _] = True
cardsInGroup [Six _, Six _, Six _, Six _] = True
cardsInGroup [Seven _, Seven _, Seven _] = True
cardsInGroup [Seven _, Seven _, Seven _, Seven _] = True
cardsInGroup [Eight _, Eight _, Eight _] = True
cardsInGroup [Eight _, Eight _, Eight _, Eight _] = True
cardsInGroup [Nine _, Nine _, Nine _] = True
cardsInGroup [Nine _, Nine _, Nine _, Nine _] = True
cardsInGroup [Ten _, Ten _, Ten _] = True
cardsInGroup [Ten _, Ten _, Ten _, Ten _] = True
cardsInGroup [Jack _, Jack _, Jack _] = True
cardsInGroup [Jack _, Jack _, Jack _, Jack _] = True
cardsInGroup [Queen _, Queen _, Queen _] = True
cardsInGroup [Queen _, Queen _, Queen _, Queen _] = True
cardsInGroup [King _, King _, King _] = True
cardsInGroup [King _, King _, King _, King _] = True
cardsInGroup [Ace _, Ace _, Ace _] = True
cardsInGroup [Ace _, Ace _, Ace _, Ace _] = True
cardsInGroup _ = False

-- cardsInSequence :: [Card] -> Bool

-- canMeld :: Hand -> Bool