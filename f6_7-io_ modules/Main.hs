{- Вариант №26
Структура данных: список массивов вещественных чисел.
Два запроса:
    1. Позволяющий получить список, упорядоченный по среднему арифметическому элементов.
    2. Позволяющий определить, есть ли компоненты списка с одинаковым количеством элементов.
-}

module Main where

import Struct
import Func
import InterfaceFunc
import System.IO
import System.Environment
import System.IO.Error
import Control.Exception

sortFunc :: [TDataStruct [Double]] -> IO ()
sortFunc list = do
    showSortMenu
    choice <- getLine
    case choice of
        "1" -> do
            putStrLn ""
            printList $ sortList (forArithmAsc) list
        "2" -> do
            putStrLn ""
            printList $ sortList (forArithmDesc) list
        _ -> do
            putStrLn ""
            putStrLn "INVALID VALUE"
            putStrLn ""
            sortFunc list

mainFunc :: [TDataStruct [Double]] -> IO ()
mainFunc list = do
    showMenu
    choice <- getLine
    case choice of
        "1" -> do
            newList <- addStruct list
            mainFunc newList
        "2" -> if ((length list) > 0)
            then do
                newList <- modifyStruct list
                mainFunc newList
            else do
                putStrLn ""
                putStrLn "Error: Empty List!"
                putStrLn ""
                mainFunc list
        "3" -> if ((length list) > 0)
            then do
                newList <- delStruct list
                mainFunc newList
            else do
                putStrLn ""
                putStrLn "Error: Empty List!"
                putStrLn ""
                mainFunc list
        "4" -> do
            putStrLn ""
            printList list
            putStrLn ""
            mainFunc list
        "5" -> if ((length list) > 0)
            then do
                putStrLn ""
                sortFunc list
                mainFunc list
            else do
                putStrLn ""
                putStrLn "Error: Empty List!"
                putStrLn ""
                mainFunc list
        "6" -> if ((length list) > 0)
            then do
                putStrLn ""
                isEqualLength list
                putStrLn ""
                mainFunc list
            else do
                putStrLn ""
                putStrLn "Error: Empty List!"
                putStrLn ""
                mainFunc list
        "7" -> if ((length list) > 0)
            then do
                putStrLn ""
                saveListToFile list
                putStrLn ""
                mainFunc list
            else do
                putStrLn ""
                putStrLn "Error: Empty List!"
                putStrLn ""
                mainFunc list
        "8" -> do
            inputFileName <- getInputFileName
            file <- openFile inputFileName ReadMode
            content <- hGetContents file
            if ((length content) > 0)
            then do
                newList <- convertStringToList content
                putStrLn ""
                putStrLn "Succesfully!"
                putStrLn ""
                hClose file
                mainFunc newList
            else do
                putStrLn ""
                putStrLn "Error: Empty File!"
                putStrLn ""
                hClose file
                mainFunc list
        "0" -> do
            putStrLn ""
        _ -> do
            putStrLn ""
            putStrLn "INVALID VALUE"
            putStrLn ""
            mainFunc list

toTry :: IO ()
toTry = do
    inputFileName <- getInputFileName
    file <- openFile inputFileName ReadMode
    hClose file
    showGreeting
    mainFunc []
    showGoodbye

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"
    | otherwise = ioError e

main :: IO ()
main = do
    argv <- getArgs
    let argc = length argv
    case argc of
        0 -> do
            putStrLn ("The program waits for the input file name and output file name as command"
                      ++ "line arguments:")
            putStrLn "First argument - name of the input file."
            putStrLn "Second argument - name of the output file."
        2 -> do
            toTry `catch` handler
        _ -> putStrLn "Error: Input file name and output file name expected!"
