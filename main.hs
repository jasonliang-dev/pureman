import System.Random (randomRIO)

pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

main :: IO ()
main = fmap lines (readFile "words.txt") >>= pick >>= print
