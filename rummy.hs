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

makeDeck :: Deck
makeDeck = [r s | s <- [Clubs .. Spades], r <- [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]]

shuffleDeck :: Deck -> Deck
shuffleDeck deck = undefined

type Hand = [Card]

data Player = Player { name :: String, hand :: Hand } deriving (Show)

