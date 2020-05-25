-- Variant 5
superFactorial :: (Integral a) => a -> a
superFactorial 0 = 1
superFactorial x
    | x < 0 = error "Invalid Value!"
    | otherwise = (factorial x) * (superFactorial (x  - 1))
        where factorial :: (Integral a) => a -> a
              factorial 0 = 1
              factorial x = x * (factorial (x - 1))

sequenceSuperFactorial :: (Integral a) => a -> [a]
sequenceSuperFactorial 0 = []
sequenceSuperFactorial n
    | n < 0 = error "Invalid Value!"
    | otherwise = [(superFactorial x) | x <- [0..n]]
