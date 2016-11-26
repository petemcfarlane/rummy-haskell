data Suit = Spades | Hearts | Diamonds | Clubs deriving (Eq, Ord, Enum)

instance Show Suit where
    show Spades = "♠"
    show Hearts = "♥"
    show Diamonds = "♦"
    show Clubs = "♣"
