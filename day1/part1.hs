import Data.List
import Data.Char

main :: IO()
main = interact (
    show .
    sum .
    map solve .
    lines
    )

solve x = read [head $ filter isDigit x, head $ filter isDigit $ reverse x]
