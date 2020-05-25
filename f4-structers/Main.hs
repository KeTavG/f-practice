{- Вариант №26
Структура данных: список массивов вещественных чисел.
Два запроса:
    1. Позволяющий получить список, упорядоченный по среднему арифметическому элементов.
    2. Позволяющий определить, есть ли компоненты списка с одинаковым количеством элементов.
-}

data TDataStruct = TDataStruct {
    arrayLength      :: Int,
    array            :: [Double],
    arithmeticalMean :: Double
    } deriving (Show)

firstStruct = TDataStruct {
    arrayLength      = length $ array firstStruct,
    array            = [3.4, 4.2, 5.5, 6],
    arithmeticalMean = (sum (array firstStruct)) / (fromIntegral (arrayLength firstStruct))
    }

secondStruct = TDataStruct {
    arrayLength      = length $ array secondStruct,
    array            = [3, 4],
    arithmeticalMean = (sum (array secondStruct)) / (fromIntegral (arrayLength secondStruct))
    }

thirdStruct = TDataStruct {
    arrayLength      = length $ array thirdStruct,
    array            = [3.3, 3.33, 3.303, 3],
    arithmeticalMean = (sum (array thirdStruct)) / (fromIntegral (arrayLength thirdStruct))
    }

pushBack :: [TDataStruct] -> TDataStruct -> [TDataStruct]
pushBack list struct = list ++ [struct]

deleteFromList :: [TDataStruct] -> Int -> [TDataStruct]
deleteFromList [] _ = error "Empty List!"
deleteFromList list number
    | (number < 1) || (number > (length list)) = error "Invalid Value!"
    | otherwise = (take (number - 1) list) ++ (drop number list)

modificationInList :: [TDataStruct] -> Int -> [Double] -> [TDataStruct]
modificationInList [] _ _ = error "Empty List!"
modificationInList _ _ [] = error "Empty Array!"
modificationInList list number newArray
    | (number < 1) || (number > (length list)) = error "Invalid Value!"
    | otherwise = replaceStruct struct{ arrayLength = length newArray, array = newArray,
                                        arithmeticalMean = (sum newArray) / (fromIntegral (length newArray))
                                      }
        where struct = head $ drop (number - 1) list
              replaceStruct newStruct = (take (number - 1) list) ++ [newStruct] ++ (drop number list)

-- Higher order function. Takes the function to sort the list (forArithmAsc, forLength).
sortList :: ([TDataStruct] -> [TDataStruct]) -> [TDataStruct] -> [TDataStruct]
sortList f list = f list

-- Sorting by arithmetical mean. Used in sortList.
forArithmAsc :: [TDataStruct] -> [TDataStruct]
forArithmAsc [] = []
forArithmAsc (x:xs) = smallerSorted ++ [x] ++ biggerSorted
    where smallerSorted = forArithmAsc [y | y <- xs, (arithmeticalMean y) <= (arithmeticalMean x)]
          biggerSorted = forArithmAsc [y | y <- xs, (arithmeticalMean y) > (arithmeticalMean x)]

forArithmDesc :: [TDataStruct] -> [TDataStruct]
forArithmDesc [] = []
forArithmDesc (x:xs) = biggerSorted ++ [x] ++ smallerSorted
    where smallerSorted = forArithmDesc [y | y <- xs, (arithmeticalMean y) <= (arithmeticalMean x)]
          biggerSorted = forArithmDesc [y | y <- xs, (arithmeticalMean y) > (arithmeticalMean x)]

-- Sorting by array length. Used in sortList.
forLength :: [TDataStruct] -> [TDataStruct]
forLength [] = []
forLength (x:xs) = smallerSorted ++ [x] ++ biggerSorted
    where smallerSorted = forLength [y | y <- xs, (arrayLength y) <= (arrayLength x)]
          biggerSorted = forLength [y | y <- xs, (arrayLength y) > (arrayLength x)]

isEqualLength :: [TDataStruct] -> Bool
isEqualLength [] = error "Empty List"
isEqualLength (x:xs)
    | (length xs) == 0 = False
    | otherwise = compareElements (sortList (forLength) (x:xs))
        where compareElements (y1:y2:ys)
                  | (arrayLength y1) == (arrayLength y2) = True
                  | (length ys) == 0 = False
                  | otherwise = compareElements (y2:ys)

testAdd :: [TDataStruct]
testAdd = pushBack [firstStruct, secondStruct] thirdStruct

testDel :: [TDataStruct]
testDel = deleteFromList [firstStruct, secondStruct, thirdStruct] 2

testMod :: [TDataStruct]
testMod = modificationInList [firstStruct, secondStruct, thirdStruct] 2 [4, 2]

testFirstReqAsc :: [TDataStruct]
testFirstReqAsc = sortList (forArithmAsc) [firstStruct, secondStruct, thirdStruct, secondStruct, firstStruct]

testFirstReqDesc :: [TDataStruct]
testFirstReqDesc = sortList (forArithmDesc) [firstStruct, secondStruct, thirdStruct, secondStruct, firstStruct]

testSecondReq :: Bool
testSecondReq = isEqualLength [firstStruct, secondStruct, thirdStruct]
