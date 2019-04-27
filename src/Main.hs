module Main where

import           Control.Monad (when)
import           Data.Char     (toLower)
import qualified Data.Set      as Set
import           System.IO     (hFlush, stdout)
import           System.Random (randomRIO)

data Model = Model
    { word    :: String
    , guesses :: Set.Set Char
    } deriving (Show)

data Msg = SetWord String | AddGuess Char

initialModel :: Model
initialModel = Model { word = "", guesses = Set.empty }

update :: Msg -> Model -> Model
update msg model =
    case msg of
        SetWord str ->
            model { word = str }
        AddGuess letter ->
            model { guesses = Set.insert letter $ guesses model }

wrongGuesses :: Set.Set Char -> String -> Int
wrongGuesses xs str = length $ Set.difference xs $ Set.fromList str

displayWord :: Set.Set Char -> String -> String
displayWord xs = foldl hideChars ""
    where
        hideChars acc x = acc ++ hideChar x
        hideChar x
            | toLower x `elem` xs = [x]
            | otherwise = "_"

display :: Int -> Set.Set Char -> String -> String
display maxGuessCount xs str =
    "Letters guessed:" ++ guessContents xs ++ "\n\n"
    ++ displayWord xs str ++ "\n\n"
    ++ show (maxGuessCount - wrongGuesses xs str) ++ " guesses left"
    where
        guessContents = foldl (\acc x -> acc ++ " " ++ [x]) ""

maxGuesses :: Int
maxGuesses = 12

game :: Model -> IO ()
game model = do
    let xs = guesses model
    let str = word model
    let toPrint = display maxGuesses xs str
    when ('_' `elem` toPrint && wrongGuesses xs str < maxGuesses) $ do
        putStrLn toPrint
        putStr "Guess a letter: "
        hFlush stdout
        letter <- getChar
        game $ update (AddGuess $ toLower letter) model

pick :: [a] -> IO a
pick xs = (xs !!) <$> randomRIO (0, length xs - 1)

main :: IO ()
main = do
    str <- lines <$> readFile "words.txt" >>= pick
    game $ update (SetWord str) initialModel
    putStrLn $ "\nThe word was " ++ str ++ "!"
