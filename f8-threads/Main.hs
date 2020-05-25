{- Вариант №18
Программа принимает от пользователя элементы целочисленного одномерного массива,
а также целочисленное значение для поиска,
затем осуществляет поиск элемента двумя любыми известными методами,
кроме прямого (линейного) поиска, и выводит результат на экран.
-}

import Func
import System.Environment
import Control.Concurrent
import Data.List

firstThread :: [Int] -> Int -> IO ()
firstThread array reqElement = do
    let index = getIndex (elemIndex reqElement array)
    if (index == -1)
    then putStrLn "Such an element does not exist!"
    else putStrLn ("Index of your element " ++ (show index))
    where
        getIndex (Just ind) = ind
        getIndex Nothing = -1

secondThread :: [Int] -> Int -> IO ()
secondThread array reqElement = do
    let index = binarySearch array reqElement
    if (index == -1)
    then putStrLn "Such an element does not exist!"
    else putStrLn ("Index of your element " ++ (show index))

mainFunc :: Int -> IO ()
mainFunc arrayLength = do
    putStrLn "Enter an array of integers ordered in non-decreasing order."
    array <- createArray arrayLength
    putStrLn "Enter the item to search for in the array."
    str <- getLine
    reqElement <- getInt str
    mvar <- newEmptyMVar
    putStrLn "Forking thread #1"
    forkFinally (firstThread array reqElement) (\_ -> putMVar mvar ())
    putStrLn "Forking thread #2"
    forkFinally (secondThread array reqElement) (\_ -> putMVar mvar ())
    takeMVar mvar
    takeMVar mvar

main :: IO ()
main = do
    argv <- getArgs
    let argc = length argv
    case argc of
        0 -> do
            putStrLn "The program expects array length as command line arguments:"
            putStrLn "First argument - array length."
        1 -> do
            let maybeLength = readMaybe (head argv) :: Maybe Int
            case maybeLength of
                Just lengthArray -> do
                    if (lengthArray > 0)
                    then mainFunc lengthArray
                    else putStrLn "The length of the array should be a positive integer (Int, >0)"
                _ -> putStrLn "The length of the array should be a positive integer (Int, >0)"
        _ -> putStrLn "Error: Array length expected!"
