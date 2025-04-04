lev :: Eq a => [a] -> [a] -> Int
lev xs [] = length xs
lev [] ys = length ys
lev (x:xs) (y:ys)
  | x == y    = lev xs ys
  | otherwise = 1 + minimum [ lev xs (y:ys)
                            , lev (x:xs) ys
                            , lev xs     ys
                            ]

main :: IO ()
main = print $ lev "hello" "world"
