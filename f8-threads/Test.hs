import Func
import Data.List
import Test.HUnit

test1 = TestCase (assertEqual "For pattern - [1, 2, 3]" True (isOrdered [1, 2, 3]))
test2 = TestCase (assertEqual "For pattern - [-123, 0, 20]" True (isOrdered [-123, 0, 20]))
test3 = TestCase (assertEqual "For pattern - [3, 23, 3]" False (isOrdered [3, 23, 3]))

test4 = TestCase (assertEqual "For pattern - [1, 2, 3] ; index - 0" 1 (getElFromArr [1, 2, 3] 0))
test5 = TestCase (assertEqual "For pattern - [1, 2, 3] ; index - 1" 2 (getElFromArr [1, 2, 3] 1))
test6 = TestCase (assertEqual "For pattern - [1, 2, 3] ; index - 2" 3 (getElFromArr [1, 2, 3] 2))

test7 = TestCase (assertEqual "For pattern - [-12, 0, 10, 33, 34, 123] ; element - -12" 0 (binarySearch [-12, 0, 10, 33, 34, 123] (-12)))
test8 = TestCase (assertEqual "For pattern - [-12, 0, 10, 33, 34, 123] ; element - 0" 1 (binarySearch [-12, 0, 10, 33, 34, 123] 0))
test9 = TestCase (assertEqual "For pattern - [-12, 0, 10, 33, 34, 123] ; element - 10" 2 (binarySearch [-12, 0, 10, 33, 34, 123] 10))
test10 = TestCase (assertEqual "For pattern - [-12, 0, 10, 33, 34, 123] ; element - 33" 3 (binarySearch [-12, 0, 10, 33, 34, 123] 33))
test11 = TestCase (assertEqual "For pattern - [-12, 0, 10, 33, 34, 123] ; element - 34" 4 (binarySearch [-12, 0, 10, 33, 34, 123] 34))
test12 = TestCase (assertEqual "For pattern - [-12, 0, 10, 33, 34, 123] ; element - 123" 5 (binarySearch [-12, 0, 10, 33, 34, 123] 123))

test13 = TestCase (assertEqual "For pattern - [-12, 0, 10, 33, 34, 123] ; element - -12" (Just 0) (elemIndex (-12) [-12, 0, 10, 33, 34, 123]))
test14 = TestCase (assertEqual "For pattern - [-12, 0, 10, 33, 34, 123] ; element - 0" (Just 1) (elemIndex 0 [-12, 0, 10, 33, 34, 123]))
test15 = TestCase (assertEqual "For pattern - [-12, 0, 10, 33, 34, 123] ; element - 10" (Just 2) (elemIndex 10 [-12, 0, 10, 33, 34, 123]))
test16 = TestCase (assertEqual "For pattern - [-12, 0, 10, 33, 34, 123] ; element - 33" (Just 3) (elemIndex 33 [-12, 0, 10, 33, 34, 123]))
test17 = TestCase (assertEqual "For pattern - [-12, 0, 10, 33, 34, 123] ; element - 34" (Just 4) (elemIndex 34 [-12, 0, 10, 33, 34, 123]))
test18 = TestCase (assertEqual "For pattern - [-12, 0, 10, 33, 34, 123] ; element - 123" (Just 5) (elemIndex 123 [-12, 0, 10, 33, 34, 123]))

test19 = TestCase (assertEqual "For pattern - []" False (isOrdered []))
test20 = TestCase (assertEqual "For pattern - [1, 2, 3] ; index - 23" 3 (getElFromArr [1, 2, 3] 23))
test21 = TestCase (assertEqual "For pattern - [] ; element - 123" (-1) (binarySearch [] 123))

test22 = TestCase (assertEqual "For pattern - [-12, 0, 10, 33, 34, 123] ; element - -12" (-1) (binarySearch [-12, 0, 10, 33, 34, 123] (-1232)))
test23 = TestCase (assertEqual "For pattern - [-12, 0, 10, 33, 34, 123] ; element - -12" Nothing (elemIndex (-1232) [-12, 0, 10, 33, 34, 123]))

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3,
                  TestLabel "test4" test4, TestLabel "test5" test5, TestLabel "test6" test6,
                  TestLabel "test7" test7, TestLabel "test8" test8, TestLabel "test9" test9,
                  TestLabel "test10" test10, TestLabel "test11" test11, TestLabel "test12" test12,
                  TestLabel "test13" test13, TestLabel "test14" test14, TestLabel "test15" test15,
                  TestLabel "test16" test16, TestLabel "test17" test17, TestLabel "test18" test18,
                  TestLabel "test19" test19, TestLabel "test20" test20, TestLabel "test21" test21,
                  TestLabel "test22" test22, TestLabel "test23" test23
                 ]

main = runTestTT tests
