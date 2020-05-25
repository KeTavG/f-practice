module Func where

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
    [(value, "")] -> Just value
    _ -> Nothing

getInt :: String -> IO Int
getInt line = do
    let maybeInt = readMaybe line :: Maybe Int
    case maybeInt of
        Just intValue -> return intValue
        _ -> do
            putStrLn "INVALID VALUE"
            putStrLn "Try again:"
            newLine <- getLine
            getInt newLine

generateArray :: [Int] -> Int -> Int -> IO [Int]
generateArray list lengthArray counter = if (lengthArray > counter)
    then do
        putStr ("array[" ++ (show counter) ++ "] = ")
        str <- getLine
        element <- getInt str
        let newList = list ++ [element]
        generateArray newList lengthArray (counter + 1)
    else return list

isOrdered :: [Int] -> Bool
isOrdered [] = False
isOrdered (x:xs)
    | (length xs) == 0 = True
    | otherwise = compareElements (x:xs)
        where
            compareElements (y1:y2:ys)
                | y1 > y2 = False
                | (length ys) == 0 = True
                | otherwise = compareElements (y2:ys)

createArray :: Int -> IO [Int]
createArray lengthArray = do
    array <- generateArray [] lengthArray 0
    if (isOrdered array)
    then return array
    else do
        putStrLn "The array is not sorted by non-decreasing!"
        putStrLn "Try again:"
        createArray lengthArray

getElFromArr :: [Int] -> Int -> Int
getElFromArr array number
    | number < 0 = head array
    | number > ((length array) - 1) = getElFromArr array ((length array) - 1)
    | number == 0 = head array
    | number > 0 = getElFromArr (tail array) (number - 1)

binarySearch :: [Int] -> Int -> Int
binarySearch array reqElement = binaryAlgorithm 0 ((length array) - 1)
    where
        binaryAlgorithm down top =
            if (down <= top)
            then if (reqElement == (getElFromArr array mid))
                 then mid
                 else if (reqElement < (getElFromArr array mid))
                      then binaryAlgorithm down (mid - 1)
                      else binaryAlgorithm (mid + 1) top
            else -1
            where
                mid = (down + top) `div` 2
