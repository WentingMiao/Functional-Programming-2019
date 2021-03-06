module hw03
import StdEnv

/*
Write a function that takes a list of numbers and
breaks it into two lists with alternating members from
the original list.

For example: [3,5,6,8,7,9] -> [ [3,6,7], [5,8,9] ]
*/


helpera :: [Int] ->[Int]
helpera [] = []
helpera [a]=[a]
helpera [a,b:c]
=[a:helpera c]

helperb :: [Int] ->[Int]
helperb [] = []
helperb [a]=[]
helperb [a,b:c]
=[b:helperb c]

splitList :: [Int]  -> [[Int]]
splitList [] =[[],[]]
splitList n
=[helpera n,helperb n]

//Start = splitList [1,2,3,4,5]
//Start = splitList [56,3,87,5,234,5,0,-4] //[[56,87,234,0],[3,5,5,-4]]
//Start = splitList [1,4..50] //[[1,7,13,19,25,31,37,43,49],[4,10,16,22,28,34,40,46]]
//Start = splitList [420] //[[420],[]]
//Start = splitList []//[[],[]]






/*
Write a function that takes a list of numbers and
adds the first element, subtracts the second element,
adds the third element, subtracts the fourth element,
in this alternating repetition.

For example: [2,3,4,5,6,7] -> 2-3+4-5+6-7 = -3
*/

alternatinghelper :: [Int] Int -> Int 
alternatinghelper [] sum = sum
alternatinghelper [a] sum  = sum+a
alternatinghelper [a,b:rest] sum
=alternatinghelper rest (sum+a-b)

alternatingSum :: [Int] -> Int
alternatingSum n
=alternatinghelper n 0 

//Start = alternatingSum [2..7] //-3
//Start = alternatingSum [45,-5,63,46,-345,4321] //-4599
//Start = alternatingSum [] //0








/*
Write a function that converts binary numbers to decimal numbers.

For example: 10010 = 2^4 + 2^1 = 18
*/

binaryhelper :: Int Int Int -> Int
binaryhelper 0 e sum = sum
binaryhelper 1 1 sum = sum+1
binaryhelper n e sum
= binaryhelper (n/10) (e*2) (sum+(e*(n rem 10)))



binaryToDecimal :: Int -> Int
binaryToDecimal n
=binaryhelper n 1 0








//Start = binaryToDecimal 10010 //18
//Start = binaryToDecimal 1010101010101 //5461






