module Func where

import Struct
import System.Environment

getInputFileName :: IO String
getInputFileName = do
    argv <- getArgs
    let fileName = head argv
    return fileName

getOutputFileName :: IO String
getOutputFileName = do
    argv <- getArgs
    let fileName = head (tail argv)
    return fileName

fileName :: FilePath
fileName = "saveFile.txt"

printList :: [TDataStruct [Double]] -> IO ()
printList [] = putStrLn "Empty List"
printList (x:xs) = printListWithCounter (x:xs) 1
    where printListWithCounter [] _ = return ()
          printListWithCounter (x:xs) counter = printStruct x counter
              where printStruct (DataStruct a) counter = do
                        putStrLn ((show counter) ++ ")")
                        putStr "Array length = "
                        print $ length a
                        putStr "Array: "
                        print a
                        putStr "Arithmetic mean of the array = "
                        print $ (sum a) / (fromIntegral (length a))
                        putStrLn ""
                        printListWithCounter xs (counter + 1)
                        return ()

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
    [(value, "")] -> Just value
    _ -> Nothing

getLength :: String -> IO Int
getLength line = do
    let maybeLength = readMaybe line :: Maybe Int
    case maybeLength of
        Just lengthArray -> do
            if (lengthArray > 0)
            then return lengthArray
            else do
                putStrLn "INVALID VALUE"
                putStrLn "Try again:"
                newLine <- getLine
                getLength newLine
        _ -> do
            putStrLn "INVALID VALUE"
            putStrLn "Try again:"
            newLine <- getLine
            getLength newLine

getDouble :: String -> IO Double
getDouble line = do
    let maybeDouble = readMaybe line :: Maybe Double
    case maybeDouble of
        Just doubleValue -> return doubleValue
        _ -> do
            putStrLn "INVALID VALUE"
            putStrLn "Try again:"
            newLine <- getLine
            getDouble newLine

generateArray :: [Double] -> Int -> Int -> IO [Double]
generateArray list lengthArray counter = if (lengthArray > counter)
    then do
        putStr ("array[" ++ (show counter) ++ "] = ")
        str <- getLine
        element <- getDouble str
        let newList = list ++ [element]
        generateArray newList lengthArray (counter + 1)
    else return list

createArray :: IO [Double]
createArray = do
    putStrLn "Enter the length of array:"
    str <- getLine
    lengthArray <- getLength str
    array <- generateArray [] lengthArray 0
    return array

pushBack :: [TDataStruct [Double]] -> TDataStruct [Double] -> [TDataStruct [Double]]
pushBack list struct = list ++ [struct]

addStruct :: [TDataStruct [Double]] -> IO [TDataStruct [Double]]
addStruct list = do
    putStrLn ""
    array <- createArray
    putStrLn ""
    return (pushBack list (DataStruct array))

getNumber :: String -> [TDataStruct [Double]] -> IO Int
getNumber line list = do
    let maybeNumber = readMaybe line :: Maybe Int
    case maybeNumber of
        Just number -> do
            if ((number > 0) && (number <= (length list)))
            then return number
            else do
                putStrLn "INVALID VALUE"
                putStrLn "Try again:"
                newLine <- getLine
                getNumber newLine list
        _ -> do
            putStrLn "INVALID VALUE"
            putStrLn "Try again:"
            newLine <- getLine
            getNumber newLine list

getLengthArray :: TDataStruct [Double] -> Int
getLengthArray (DataStruct list) = length list

modificationInList :: [TDataStruct [Double]] -> Int -> [Double] -> [TDataStruct [Double]]
modificationInList [] _ _ = error "Empty List!"
modificationInList _ _ [] = error "Empty Array!"
modificationInList list number newArray
    | (number < 1) || (number > (length list)) = error "Invalid Value!"
    | otherwise = replaceStruct (DataStruct newArray)
        where replaceStruct newStruct = (take (number - 1) list) ++ [newStruct] ++ (drop number list)

modifyStruct :: [TDataStruct [Double]] -> IO [TDataStruct [Double]]
modifyStruct list = do
    putStrLn ""
    putStrLn "Enter the structure number to modify:"
    str <- getLine
    number <- getNumber str list
    array <- generateArray [] (getLengthArray (head (drop (number - 1) list))) 0
    putStrLn ""
    return $ modificationInList list number array

deleteFromList :: [TDataStruct [Double]] -> Int -> [TDataStruct [Double]]
deleteFromList [] _ = error "Empty List!"
deleteFromList list number
    | (number < 1) || (number > (length list)) = error "Invalid Value!"
    | otherwise = (take (number - 1) list) ++ (drop number list)

delStruct :: [TDataStruct [Double]] -> IO [TDataStruct [Double]]
delStruct list = do
    putStrLn ""
    putStrLn "Enter the structure number to delete:"
    str <- getLine
    number <- getNumber str list
    putStrLn ""
    return $ deleteFromList list number

-- Higher order function. Takes the function to sort the list (forArithmAsc, forLength).
sortList :: ([TDataStruct [Double]] -> [TDataStruct [Double]]) -> [TDataStruct [Double]] -> [TDataStruct [Double]]
sortList f list = f list

-- Sorting by arithmetical mean. Used in sortList.
forArithmAsc :: [TDataStruct [Double]] -> [TDataStruct [Double]]
forArithmAsc [] = []
forArithmAsc (x:xs) = smallerSorted ++ [x] ++ biggerSorted
    where arithMean arr = (sum arr) / (fromIntegral (length arr))
          smallerSorted = forArithmAsc (filter (\y -> (fmap arithMean y) <= (fmap arithMean x)) xs)
          biggerSorted = forArithmAsc (filter (\y -> (fmap arithMean y) > (fmap arithMean x)) xs)

forArithmDesc :: [TDataStruct [Double]] -> [TDataStruct [Double]]
forArithmDesc [] = []
forArithmDesc (x:xs) = biggerSorted ++ [x] ++ smallerSorted
    where arithMean arr = (sum arr) / (fromIntegral (length arr))
          smallerSorted = forArithmDesc (filter (\y -> (fmap arithMean y) <= (fmap arithMean x)) xs)
          biggerSorted = forArithmDesc (filter (\y -> (fmap arithMean y) > (fmap arithMean x)) xs)

-- Sorting by array length. Used in sortList.
forLength :: [TDataStruct [Double]] -> [TDataStruct [Double]]
forLength [] = []
forLength (x:xs) = smallerSorted ++ [x] ++ biggerSorted
    where
        smallerSorted = forLength (filter (\y -> (fmap length y) <= (fmap length x)) xs)
        biggerSorted = forLength (filter (\y -> (fmap length y) > (fmap length x)) xs)

isEqualLength :: [TDataStruct [Double]] -> IO ()
isEqualLength [] = putStrLn "Empty List"
isEqualLength (x:xs)
    | (length xs) == 0 = putStrLn "False"
    | otherwise = compareElements (sortList (forLength) (x:xs))
        where
            compareElements (y1:y2:ys)
                | (fmap length y1) == (fmap length y2) = putStrLn "True"
                | (length ys) == 0 = putStrLn "False"
                | otherwise = compareElements (y2:ys)

convertArrayToString :: Int -> [Double] -> Int -> String
convertArrayToString lengthArray array counter
    | (lengthArray == counter) = (show (head array)) ++ divider
    | otherwise = (show (head array)) ++ divider
                  ++ (convertArrayToString lengthArray (tail array) (counter + 1))
        where
            divider = ";"

getArray :: TDataStruct [Double] -> [Double]
getArray (DataStruct array) = array

convertListToString :: [TDataStruct [Double]] -> String -> IO String
convertListToString list string = if ((length list) == 1)
    then return str
    else (convertListToString (tail list) str)
        where
            divider = ";"
            structDivider = "\n"
            struct = head list
            str = string ++ (show (getLengthArray struct)) ++ divider
                  ++ (convertArrayToString (getLengthArray struct) (getArray struct) 1) ++ structDivider

saveListToFile :: [TDataStruct [Double]] -> IO ()
saveListToFile list = do
    str <- convertListToString list ""
    outputFileName <- getOutputFileName
    writeFile outputFileName str
    putStrLn "Succesfully!"

split :: Char -> String -> [String]
split _ "" = []
split delim str = word:(split delim rest)
    where
        word = takeWhile (/= delim) str
        rest = drop ((length word) + 1) str

splitListOfString :: Char -> [String] -> [[String]]
splitListOfString delim listStr
    | ((length listStr) == 1) = [split delim (head listStr)]
    | otherwise = [split delim (head listStr)] ++ (splitListOfString delim (tail listStr))

readArray :: Int -> [String] -> Int -> [Double]
readArray lengthArray list counter
    | (lengthArray == counter) = [element]
    | otherwise = [element] ++ (readArray lengthArray (tail list) (counter + 1))
        where
            element = read (head list) :: Double

convertListToArray :: [String] -> [Double]
convertListToArray list = readArray lengthArray (tail list) 1
    where
        lengthArray = read (head list) :: Int

convertContentToList :: [[String]] -> [TDataStruct [Double]] -> [TDataStruct [Double]]
convertContentToList tmpList listOfStruct
    | ((length tmpList) == 1) = pushBack listOfStruct struct
    | otherwise = convertContentToList (tail tmpList) (pushBack listOfStruct struct)
        where
            struct = DataStruct (convertListToArray (head tmpList))

convertStringToList :: String -> IO [TDataStruct [Double]]
convertStringToList string = return listOfStruct
    where
        listOfStrings = split '\n' string
        tmpList = splitListOfString ';' listOfStrings
        listOfStruct = convertContentToList tmpList []
