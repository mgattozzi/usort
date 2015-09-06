import Data.Char
import Data.List

main :: IO ()
main = do
    file <- getContents
    putStr $ usort file

quicksort :: (Ord a) => [a] ->[a]
quicksort [] = []
quicksort (x:xs) =
        let smaller = filter (<= x) xs
            larger  = filter (> x) xs
        in quicksort smaller ++ [x] ++ quicksort larger

usort :: String -> String
usort x = unlines $ nub $ quicksort $ lines x
