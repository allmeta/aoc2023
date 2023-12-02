import Data.List
import Data.Char
import Data.Maybe

main :: IO()
main = interact (
    show .
    sum .
    map solve .
    lines
    )

ns = words "one two three four five six seven eight nine"

solve x = read [findDigit x ns, findDigit (reverse x) (map reverse ns)]

findDigit :: String -> [String] -> Char
findDigit [] _ = '0'
findDigit y@(x:xs) ns
  | isDigit x = x
  | otherwise = case foldr isPrefix Nothing ns of
                  Nothing -> findDigit xs ns
                  Just n -> head $ show $ n + 1
      where 
        isPrefix v Nothing = if v `isPrefixOf` y then elemIndex v ns else Nothing
        isPrefix v n = n
