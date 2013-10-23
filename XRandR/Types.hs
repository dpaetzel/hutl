module Util.XRandR.Types where


import Data.List


data Output = Output
    { _name  :: String
    -- , _config :: Maybe OutputConfig
    , _state :: OutputState
    }

instance Show Output where
    show (Output name state) = unwords ["--output", name, show state]


data OutputState
    = Disconnected
    | Connected
    | Configured
    { _mode     :: Mode
    , _position :: Coords
    , _rotation :: Rotation
    }

instance Show OutputState where
    show (Configured mode position rotation) = unwords
        [ show mode
        , "--pos",      showCoords position
        , "--rotation", show rotation
        ]
        where
            showCoords (x, y) = concat [show x, "x", show y]
    show _ = "--off"

-- is there a nice way?
instance Eq OutputState where
    Disconnected    == Disconnected    = True
    Connected       == Connected       = True
    (Configured {}) == (Configured {}) = True
    _               == _               = False


data Mode
    = Auto
    | Mode Coords

instance Show Mode where
    show Auto = "--auto"
    show (Mode coords) = unwords ["--mode", showCoords coords]
        where
            showCoords (x, y) = concat [show x, "x", show y]


data Rotation
    = RNormal
    | RLeft
    | RRight
    | RInverted

instance Show Rotation where
    show RNormal   = "normal"
    show RLeft     = "left"
    show RRight    = "right"
    show RInverted = "inverted"

toRotation :: String -> Rotation
toRotation "normal"   = RNormal
toRotation "left"     = RLeft
toRotation "right"    = RRight
toRotation "inverted" = RInverted
toRotation _          = RNormal


type Coords = (Int, Int)
