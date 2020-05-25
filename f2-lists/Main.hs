--lab2_var3
distributeFourArguments :: a -> a -> a -> a -> [[a]]
distributeFourArguments ar1 ar2 ar3 ar4 = [[ar1, ar2], [ar3, ar4]]

distributeTwoArguments :: a -> a -> [[a]]
distributeTwoArguments ar1 ar2 = [[ar1], [ar2]]

distributeOneArguments :: a -> [[a]]
distributeOneArguments ar1 = [[ar1]]

distributeArguments :: [a] -> [[a]]
distributeArguments [] = [[]]
distributeArguments list
    | (length list) == 1 = (distributeOneArguments (head list))
    | (length list) == 2 = (distributeTwoArguments (head list) (head(tail list)))
    | (length list) == 4 = (distributeFourArguments (head list) (head(tail list)) (head(tail(tail list))) (head(tail(tail(tail list)))))
    | otherwise = []
