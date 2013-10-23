module Util.XRandR
    ( condChange
    , onlyConnected
    , connectedPorts
    , change
    , current
    )

    where


import Control.Applicative hiding (many, (<|>), optional)
import Control.Arrow
import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import System.Process (readProcess, system)
import Text.ParserCombinators.Parsec

import Util.XRandR.Types


main = condChange
    [
        (onlyConnected ["HDMI-0", "DVI-0"],
            [ Output
                { _name  = "HDMI-0"
                , _state = Configured
                    { _mode     = Mode (1680, 1050)
                    , _position = (0, 0)
                    , _rotation = RNormal
                    }
                }
            , Output
                { _name = "DVI-0"
                , _state = Configured
                    { _mode     = Mode (1280, 1024)
                    , _position = (1680, 0)
                    , _rotation = RLeft
                    }
                }
            ])
    ]
    -- join . fmap sequence_ . fmap (map print) $ current



condChange :: [(IO Bool, [Output])] -> IO ()
condChange [] = return ()
condChange xs = doWhen . map (second change) $ xs


-- checks whether the ports are the only connected
onlyConnected :: [String] -> IO Bool
onlyConnected ports = do
    c <- connectedPorts
    return $ (length ports == length c) && all (`elem` c) ports


connectedPorts :: IO [String]
connectedPorts = fmap (map _name . filter connected) current
    where
        connected = (`elem` [Connected, Configured Auto (0, 0) RNormal]) . _state


change :: [Output] -> IO ()
change news = do
    n <- newConfig news
    -- print . unwords . map show $ n
    system . unwords . ("xrandr" :) . map show $ n
    return ()

    where
        newConfig :: [Output] -> IO [Output]
        newConfig [] = current
        newConfig news = unionBy ((==) `on` _name) news <$> current


current :: IO [Output]
current = do
    s <- readProcess "xrandr" [] ""
    let output = parse outputParser "outputParser" s
    -- print output
    return $ either (const []) id output

    where
        outputParser = do
            manyTill anyChar newline
            many1 lineParser

        -- consumes lines that start with white space (the lines contained in
        -- the list of supported modes)
        modeLines :: Parser ()
        modeLines = optional (many1 $ many1 space >> manyTill anyChar newline)
        nameParser :: Parser String
        nameParser = many1 $ alphaNum <|> char '-'
        connectedParser :: Parser Bool
        connectedParser = do
            state <- string "connected" <|> string "disconnected"
            return $ state == "connected"
        onParser :: Parser Bool
        onParser = do
            c <- anyChar
            return $ c /= '('
        modeParser :: Parser Coords
        modeParser = do
            x <- many1 digit
            char 'x'
            y <- many1 digit
            return (read x, read y)
        positionParser :: Parser Coords
        positionParser = do
            char '+'
            x <- many1 digit
            char '+'
            y <- many1 digit
            return (read x, read y)
        rotationParser :: Parser Rotation
        rotationParser = fmap toRotation $ many1 lower <|> string "("
        -- lineParser :: Parser Output
        lineParser = do
            name <- nameParser
            space
            connected <- connectedParser
            space
            on <- lookAhead onParser

            if connected && on
            then do
                mode <- modeParser
                position <- positionParser
                space
                rotation <- rotationParser
                manyTill anyChar newline

                modeLines

                return Output
                    { _name  = name
                    , _state = Configured
                        { _mode     = Mode mode
                        , _position = position
                        , _rotation = rotation
                        }
                    }
            else do
                manyTill anyChar newline

                modeLines

                return Output
                    { _name  = name
                    , _state = if connected
                        then Connected
                        else Disconnected
                    }


doWhen :: Monad m => [(m Bool, m ())] -> m ()
doWhen = mapM_ run

    where
        run :: Monad m => (m Bool, m ()) -> m ()
        run x = do
            cond <- fst x
            when cond $ snd x
