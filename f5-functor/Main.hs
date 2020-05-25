{- Вариант №26
Структура данных: список массивов вещественных чисел.
Два запроса:
    1. Позволяющий получить список, упорядоченный по среднему арифметическому элементов.
    2. Позволяющий определить, есть ли компоненты списка с одинаковым количеством элементов.
-}

data TDataStruct a = DataStruct a deriving (Show, Eq, Ord)

instance Functor TDataStruct where
    fmap f (DataStruct a) = DataStruct (f a)

instance Applicative TDataStruct where
    pure a = DataStruct a
    (DataStruct f) <*> (DataStruct a) = DataStruct (f a)

firstStruct :: TDataStruct [Double]
firstStruct = DataStruct [3.4, 4.2, 5.5, 6]

secondStruct :: TDataStruct [Double]
secondStruct = DataStruct [3, 4]

thirdStruct :: TDataStruct [Double]
thirdStruct = DataStruct [3.3, 3.33, 3.303, 3]

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

pushBack :: [TDataStruct [Double]] -> TDataStruct [Double] -> [TDataStruct [Double]]
pushBack list struct = list ++ [struct]

deleteFromList :: [TDataStruct [Double]] -> Int -> [TDataStruct [Double]]
deleteFromList [] _ = error "Empty List!"
deleteFromList list number
    | (number < 1) || (number > (length list)) = error "Invalid Value!"
    | otherwise = (take (number - 1) list) ++ (drop number list)

modificationInList :: [TDataStruct [Double]] -> Int -> [Double] -> [TDataStruct [Double]]
modificationInList [] _ _ = error "Empty List!"
modificationInList _ _ [] = error "Empty Array!"
modificationInList list number newArray
    | (number < 1) || (number > (length list)) = error "Invalid Value!"
    | otherwise = replaceStruct (DataStruct newArray)
        where replaceStruct newStruct = (take (number - 1) list) ++ [newStruct] ++ (drop number list)

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
    where smallerSorted = forLength (filter (\y -> (fmap length y) <= (fmap length x)) xs)
          biggerSorted = forLength (filter (\y -> (fmap length y) > (fmap length x)) xs)

isEqualLength :: [TDataStruct [Double]] -> Bool
isEqualLength [] = error "Empty List"
isEqualLength (x:xs)
    | (length xs) == 0 = False
    | otherwise = compareElements (sortList (forLength) (x:xs))
        where compareElements (y1:y2:ys)
                  | (fmap length y1) == (fmap length y2) = True
                  | (length ys) == 0 = False
                  | otherwise = compareElements (y2:ys)

testAdd :: IO ()
testAdd = printList $ pushBack [firstStruct, secondStruct] thirdStruct

testDel :: IO ()
testDel = printList $ deleteFromList [firstStruct, secondStruct, thirdStruct] 2

testMod :: IO ()
testMod = printList $ modificationInList [firstStruct, secondStruct, thirdStruct] 2 [4, 2]

testFirstReqAsc :: IO ()
testFirstReqAsc = printList $ sortList (forArithmAsc) [firstStruct, secondStruct, thirdStruct, secondStruct, firstStruct]

testFirstReqDesc :: IO ()
testFirstReqDesc = printList $ sortList (forArithmDesc) [firstStruct, secondStruct, thirdStruct, secondStruct, firstStruct]

testSecondReq :: Bool
testSecondReq = isEqualLength [firstStruct, secondStruct, thirdStruct]
