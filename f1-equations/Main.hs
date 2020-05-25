discriminant :: Double -> Double -> Double -> Double
discriminant a b c = b * b - 4 * a * c

linearEquation :: Double -> Double -> IO ()
linearEquation a b
    | a == 0 && b == 0 = print $ "Infinite number of roots."
    | a == 0 = print $ "It is not an equation."
    | b == 0 = print $ [0]
    | otherwise = print $ (-b / a):[]

quadraticEquation :: Double -> Double -> Double -> IO ()
quadraticEquation a b c
    | a == 0 = (linearEquation b c)
    | b == 0 && c == 0 = print $ [0]
    | b == 0 && (-c / a) < 0 = print $ "It is not an equation."
    | b == 0 = print $ (sqrt (-c / a)):(-sqrt (-c / a)):[]
    | (discriminant a b c) < 0 = print $ "The equation has no real roots."
    | (discriminant a b c) == 0 = print $ ((-b) / (2 * a)):[]
    | otherwise = print $ ((-b - sqrt (discriminant a b c)) / (2 * a)):((-b + sqrt (discriminant a b c)) / (2 * a)):[]
