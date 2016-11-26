data Suit = Spades | Hearts | Diamonds | Clubs deriving (Eq, Ord, Enum)

instance Show Suit where
    show Spades = "♠"
    show Hearts = "♥"
    show Diamonds = "♦"
    show Clubs = "♣"

data Card = Ace Suit | King Suit | Queen Suit | Jack Suit | Ten Suit | Nine Suit | Eight Suit
            | Seven Suit | Six Suit | Five Suit | Four Suit | Three Suit | Two Suit

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

