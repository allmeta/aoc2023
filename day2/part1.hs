import Data.List.Split
import Data.Char
import Data.Maybe
import Debug.Trace

main :: IO()
main = interact (
    show .
    sum .
    map solve .
    lines
    )

possible "red" = 12
possible "green" = 13
possible "blue" = 14

solve game = if all isPossible rounds then traceShowId n else 0
  where
    (id:r':_) = splitOn ":" game
    rounds = splitOn ";" r'
    n = read $ last $ words id

isPossible round = all (isLess . words) cubes
  where cubes = splitOn "," round

isLess [n,color] = read n <= possible color
