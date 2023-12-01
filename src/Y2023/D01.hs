module Y2023.D01 (solutions) where

solutions :: [IO ()]
solutions = [s1]

s1 :: IO ()
s1 = interact (unlines . fmap reverse . lines)
