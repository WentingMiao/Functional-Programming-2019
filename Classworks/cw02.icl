module third
import StdEnv
// Reverse the list recurievely
// f1 :: [Int] -> [Int]
// Start = f1 [] // []
// Start = f1 [1,2,3,4,5] // [5,4,3,2,1]

//1 st
rev :: [Int] -> [Int]
rev [x:y]
|isEmpty y =[x]
|otherwise =(rev y) ++ [x]


f1 :: [Int] -> [Int]
f1 aList
| isEmpty aList =[]
=[last aList:f1(init aList)]

//Start=rev[1,2,3,4,5]




// Generate every odd number from 1 to n (n is function parameter)
// f2 :: Int -> [Int]
// Start = f2 1  // [1]
// Start = f2 10 // [1,3,5,7,9]
// Start = f2 (~4) // []

f2 :: Int -> [Int]
f2 n
| n <=0 =[]
=filter isOdd[1..n]

f2 :: Int -> [Int]
f2 0 =[]
f2 n
| isOdd n =[n:f2 (n-1)]
=f2 (n-1)

Start = f2 10
// Replicate the elements of a list a given number of times. 
// If list is [1,2,3] and given number 2 -> [1,1,2,2,3,3]
// f3 :: [Int] Int -> [Int]
// Start = f3 [1,1,3,4] 3 // [1,1,1,1,1,1,3,3,3,4,4,4]
// Start = f3 [1,2,3] 2 // [1,1,2,2,3,3]
// Start = f3 [] 100 // []
// Start = f3 [1..] (~45) // []




f3 :: [Int] Int -> [Int]
f3 aList num
| num <= 0 =[] 
| isEmpty aList = []
=(repeatn num(hd aList))++ f3 (tl aList) num


f3oux :: [Int] Int -> [Int]
f3oux [x:y] num
| isEmpty y =repeatn num(x)
=(repeatn num(x))++ f3oux y num

//Start = f3oux [1,2,3,4] 5



