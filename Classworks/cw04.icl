module cw04g4
import StdEnv

// Generate list of Fibonacci numbers which are less than given n and are even.

Fib :: Int -> Int
Fib 0 = 0
Fib 1 = 1
Fib m
= Fib (m-1)+ Fib (m-2)

f1 :: Int -> [Int]
f1 0 =[]
f1 n = [Fib x\\x<-[1..n]|(isEven (Fib x))&&((Fib x)<n)]
A
Fib2 :: Int Int Int -> Int 
Fib2 a b 0 =a
Fib2 a b c = Fib2 b (a+b) (c-1)

f2 :: Int -> [Int]
f2 0 =[]
f2 n = [x\\x<- [Fib2 1 1 x\\x<-[1..n]|(isEven (Fib2 1 1 x))]]|x<n]




//Start = f1 10 // [2,8]
//Start = f1 1000 // [2,8,34,144,610]
//Start = f1 100000 // [2,8,34,144,610,2584,10946,46368]A
//Start = f1 1000000000 // [2,8,34,144,610,2584,10946,46368,196418,832040,3524578,14930352,63245986,267914296]
//Start = f1 10000000000 // [2,8,34,144,610,2584,10946,46368,196418,832040,3524578,14930352,63245986,267914296,1134903170,4807526976]

//splitList ourList = [ [x\\x<-ourList & y<-[1..(length ourList)|isOdd y]], [x\\x<-ourList & y<-[1..(length ourList)|isEven y]] ]
firsts bigList 
= [a\\[a:b]<-bigList]
something bigList = [ x^2\\x<-[a\\[a:b]<-bigList ] ]
//Start = something [[1,2,3],[2,6,8],[8,7]]
//Start = f2 10 // [2,8],
//Start = f3 1000 // [2,8,34,144,610]
//Start = f2 100000 // [2,8,34,144,610,2584,10946,46368]
//Start = f2 1000000000 // [2,8,34,144,610,2584,10946,46368,196418,832040,3524578,14930352,63245986,267914296]
//Start = f2 10000000000 // [2,8,34,144,610,2584,10946,46368,196418,832040,3524578,14930352,63245986,267914296,1134903170,4807526976]


filter p [] = []
filter p [x:xs]
| p x = filter p xs 
= [x] ++ filter p xs 
odd x = not (not (not (isOdd x)))
Start = filter odd [1..10]





// Define function myLength, which returns length of a list
myLength :: [Int] -> Int 
myLength list
= foldr f 0 list
where f x y = y+1


// Start = myLength [] // 0
//Start = myLength2 [1,2,3] // 3
// Start = myLength (take 100 [1..]) // 100
// Start = myLength [1..] // Heap full



// Define function "reverse" using foldr

myReverse :: [Int] -> [Int]
myReverse []=[]
myReverse [x]=[x]
myReverse [x:xs]
= foldr (\x xs = xs ++[x]) [] [x:xs] 



//Start = myReverse [1,2,3,4,5,6,7,8] // [8,7,6,5,4,3,2,1]
//Start = myReverse [] // []
// Start = myReverse [1] // [1]









