module InterfaceFunc where

showGreeting :: IO ()
showGreeting = do
    putStrLn ""
    putStrLn "|-------------------------------------------------------------------------|"
    putStrLn "|Welcome to the program for working with a list of arrays of real numbers.|"
    putStrLn "|-------------------------------------------------------------------------|"
    putStrLn ""

showMenu :: IO ()
showMenu = do
    putStrLn "|-------------------------------------------------------------------------|"
    putStrLn "|List of available operations:                                            |"
    putStrLn "|1) Add structure                                                         |"
    putStrLn "|2) Modify structure                                                      |"
    putStrLn "|3) Delete structure                                                      |"
    putStrLn "|4) Read list                                                             |"
    putStrLn "|5) Get an ordered list                                                   |"
    putStrLn "|6) Find out if there are list components with the same number of elements|"
    putStrLn "|7) Save into file                                                        |"
    putStrLn "|8) Load from file                                                        |"
    putStrLn "|0) Exit from the program                                                 |"
    putStrLn "|-------------------------------------------------------------------------|"
    putStrLn ""
    putStrLn "Your choice?"
    return ()

showGoodbye :: IO ()
showGoodbye = do
    putStrLn "|-------------------------------------------------------------------------|"
    putStrLn "|                                Goodbye!                                 |"
    putStrLn "|-------------------------------------------------------------------------|"
    putStrLn ""
    return ()

showSortMenu :: IO ()
showSortMenu = do
    putStrLn "|-------------------------------------------------------------------------|"
    putStrLn "|List of available operations:                                            |"
    putStrLn "|1) Sort in ascending order                                               |"
    putStrLn "|2) Sort in descending order                                              |"
    putStrLn "|-------------------------------------------------------------------------|"
    putStrLn ""
    putStrLn "Your choice?"
    return ()
