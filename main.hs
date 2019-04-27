import           Control.Monad (when)
import           Data.Char     (toLower)
import qualified Data.Set      as Set
import           System.Random (randomRIO)

data Model =
    Model
        { word    :: String
        , guesses :: Set.Set Char
        } deriving (Show)

data Msg = SetWord String | AddGuess Char

initialModel :: Model
initialModel = Model { word = "", guesses = Set.empty }

update :: Msg -> Model -> Model
update msg model =
    case msg of
        SetWord word ->
            model { word = word }
        AddGuess letter ->
            model { guesses = Set.insert letter $ guesses model }

displayWord :: Set.Set Char -> String -> String
displayWord guesses = foldl (\acc x -> acc ++ if toLower x `elem` guesses then [x] else "_") ""

display :: Int -> Set.Set Char -> String -> String
display maxGuesses guesses word =
    "Letters guessed:" ++ guessContents guesses ++ "\n\n"
    ++ displayWord guesses word ++ "\n\n"
    ++ show (maxGuesses - length guesses) ++ " guesses left"
    where guessContents = foldl (\acc x -> acc ++ " " ++ [x]) ""

maxGuesses :: Int
maxGuesses = 10

game :: Model -> IO ()
game model = do
    let xs = guesses model
    let str = display maxGuesses xs (word model)
    when ('_' `elem` str && length xs < maxGuesses) $ do
        putStrLn str
        putStr "Guess a letter: "
        (letter:_) <- getLine
        game $ update (AddGuess $ toLower letter) model

pick :: [a] -> IO a
pick xs = (xs !!) <$> randomRIO (0, length xs - 1)

main :: IO ()
main = do
    word <- lines <$> readFile "words.txt" >>= pick
    game $ update (SetWord word) initialModel
    putStrLn $ "\nThe word was " ++ word ++ "!"
