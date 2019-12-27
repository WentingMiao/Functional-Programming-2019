module midterm
import StdEnv
import StdOverloaded
/*
1.
Given a list of sublists of Int
Write a function which returns a list containing
the minimum of every sublist
You should use foldr when finding the minimum.
Ignore empty sublists.
*/

isSmaller :: Int Int -> Int
isSmaller x y
| x<y = x
= y

GetMin :: [[Int]] -> [Int]
GetMin [[]]= []
GetMin listOfList
= [ foldr (\ x y = (isSmaller x y)) a b\\[a:b]<-listOfList]


//Start = foldr (\ x y = (isSmaller x y)) 1 [1,5,36,8]
//Start = GetMin [[42, 420], [24, 240]] // [42, 24]
//Start = GetMin [[], [1], [2,3], [4,5,6]] // [1, 2, 4]
//Start = GetMin [[], [1], [2, ~3], [4, ~5, 6]] // [1, -3, -5]
//Start = GetMin [[]] // []




/*
2.
Given a list of tuples, return a single tuple which is
the sum of all tuple (x,y) in which x,y have the same parity (odd, odd)/(even, even)
minus the sum of all tuple (x,y) in which x,y dont have the same parity (odd, even)/(even, odd)
For example:
TupleSum [(1, 2), (4, 4)]
(4,4) has the same parity. We take that and subtract (1,2), which has opposite parity.
*/
TupleSumhelper :: [(Int, Int)] (Int,Int) -> (Int, Int)
TupleSumhelper [] (x,y) = (x,y)
TupleSumhelper [(a,b):rest] (x,y)
| ((isOdd a && isOdd b )||(isEven a && isEven b))== True = TupleSumhelper rest (x+a,y+b)
= TupleSumhelper rest (x - a,y - b)

TupleSum :: [(Int, Int)] -> (Int, Int)
TupleSum [] =(0,0)
TupleSum aList
=TupleSumhelper aList (0,0)

//Start = TupleSum [] // (0,0)
//Start = TupleSum [(1, 2)] // (-1, -2)
//Start = TupleSum [(1, 2), (4, 4)] // (3, 2)
//Start = TupleSum [(2, 6),(3, 4),(1, 2),(5, 9),(3, 6)] //(0,3)


/*
3.
Write a function that takes a list of tuples,
each tuple consisting of a predicate function and
a list of Int.
Return a list containing the sum of the sublists
where all elements return True for the predicate.
For example:
Start = conditionalFun [(isEven,[1..10]),(isOdd,[1,3,5,7]),(((<)3),[5..10])]
[1..10] does not return True to isEven for all elements.
[1,3,5,7] returns True for isOdd for all elements. We sum it to 16, add to list.
[5..10] returns True for greater than 3 for all elements. We sum to 45, add to list.
*/



conditionalFun :: [((Int->Bool),[Int])] -> [Int]
conditionalFun [] = []
conditionalFun [(fun,aList):rest]
| and[fun x \\ x <- aList] =[sum aList:conditionalFun rest]
=conditionalFun rest

//Start = conditionalFun [(isEven,[1..10]),(isOdd,[1,3,5,7]),(((<)3),[5..10])] //[16,45]
//Start = conditionalFun [((\x = True),[4,7..35]),((\x = False),[1..])] //[209]
//Start = conditionalFun [(isEven,[]),(isOdd,[])] //[0,0]
//Start = conditionalFun [] //[]


/*
4.
Write a function which takes a list of numbers and returns a list
containing only the palindromes.
A palindrome is a number or word which is identical when
read backwards and forwards.
For example: 123 is NOT a palindrome. 12321 is a palindrome.
*/

itsPalidromes :: Int -> Int
itsPalidromes n
= toInt(toString[x\\x<-(reverse[x\\x<-:(toString n)])])

itsPalidromes2 :: Int Int-> Int
itsPalidromes2 0 x = x
itsPalidromes2 n x
= itsPalidromes2 (n/10) (x*10 + (n rem 10))

Palindromes::[Int]->[Int]
Palindromes aList
=[x\\x<-aList |x == (itsPalidromes x)]



//Start = Palindromes [1,212,43,55,727,123,100] // [1,212,55,727]
//Start = Palindromes [76,89,1223,998]//[]
//Start = Palindromes []//[]
//Start = Palindromes [33]//[33]


/*
5.
Write a function which generates a list of the first n leap years starting from
a year x. If either of the arguments is negative output an empty list.
A leap year is divisible by 4 but is NOT divisible by 100 UNLESS it is divisible by 400
From Wikipedia:
if (year is not divisible by 4) then (it is a common year)
else if (year is not divisible by 100) then (it is a leap year)
else if (year is not divisible by 400) then (it is a common year)
else (it is a leap year)
*/

isLeap :: Int -> Bool
isLeap n 
| n rem 4 <> 0 = False
| n rem 100 <> 0 =True
| n rem 400 <> 0 =False
=True


LeapYears :: Int Int->[Int]
LeapYears start n 
| start < 0 = []
| n < 0 =[]
= take n [x\\x<-[start..]|isLeap x]

//Start=LeapYears 1999 4 // [2000,2004,2008,2012]
//Start = LeapYears 1804 7 //[1808,1812,1816,1820,1824,1828,1832]
//Start = LeapYears -2000 4 //[]
//Start = LeapYears 2000 -9//[]


/*
6.
Write a function that takes a number and determine whether it is a perfect number or not!
A perfect number is a natural number that equals the sum of all its proper divisors.
A proper divisor is every divisor of a number excluding the number itself.
Example : isPerfect 6 // True
(Because the proper divisors of 6 are 1, 2 and 3 so their sum is equal
to 6 so it is true)
*/

divisor :: Int Int-> [Int]
divisor n 1 = [1]
divisor n x
| n <=1 = []
| n rem x == 0 =[x :divisor n (x-1)]
=divisor n (x-1)


isPerfect :: Int -> Bool
isPerfect 0 =False
isPerfect n
| sum (divisor n (n-1)) == n = True
=False 


//Start = isPerfect 6 //True
//Start = isPerfect 496 //True
//Start = isPerfect 11//False
//Start = isPerfect 1 //False
//Start = isPerfect 0 //False
//Start = isPerfect -1 // False


/*
7.
Given two integers, return a list of all common divisors
of two intergers (excluding 1)
*/
divisorshelper::Int Int Int-> [Int]
divisorshelper x y 1 = []
divisorshelper x y n
| (y rem n == 0)&& (x rem n ==0) =  divisorshelper x y (n-1) ++[n]
= divisorshelper x y (n-1)

divisors::Int Int -> [Int]
divisors x y
| x > y = divisorshelper x y y
= divisorshelper x y x

//Start = divisors 6 12 //[2,3,6]
//Start = divisors 7 12 //[]
//Start = divisors 9 15 //[3]
//Start = divisors 128 64 //[2,4,8,16,32,64]


/*
8.
Given a list of intergers
find all the cube numbers(n^3) and write (n) to the first list; for example 8 -> 2
(A cube number is a number that is the product of three numbers which are the same)
find all the numbers which are powers of 2 (2^n) and write the exponent n to the
second list; for example 64 -> 6
*/

findCube :: Int Int -> Int
findCube n x
| x > n = -1
| n == x*x*x = x
=findCube n (x+1)


isPower :: Int Int -> Int
isPower n x
| x > toInt( sqrt(toReal(n))) = -1
| n == 2^x = x
=isPower n (x+1)

/*cubes2::[Int]->([Int],[Int])
cubes2 aList
= (filter ((<>) -1) [findCube x 1\\ x<-aList],filter ((<>) -1) [isPower x 0 \\ x<-aList])
*/

max :: Int [Int] -> Int
max m [] = m
max m [x:y]
| m < x = max x y
=max m y

findCube2 :: [Int] -> [Int]
findCube2 aList
= [x\\x <- [0..(max (hd aList) (tl aList))] |( isMember (x^3) aList )]

isPower2 :: [Int]-> [Int]
isPower2 aList
= [x\\x <- [0..(max (hd aList) (tl aList))]| isMember (2^x) aList]

cubes2::[Int]->([Int],[Int]) 
cubes2 aList 
= (findCube2 aList,isPower2 aList)

//Start = cubes2 [64, 16, 24, 15, 1 , 8] //([4,1,2],[6,4,0,3])
//Start = cubes2 [1..10] //([1,2],[0,1,2,3])
//ZStart = cubes2 [25..60] //([3],[5])



/*
9.
Write a function that will take a list of Integers and 
will return a list of tuples (a,b) where b is every prime index of the given 
list and a is the value of the list at that index.
Ignore 1 as a prime.
*/

factor :: Int Int -> [Int]
factor n 1 =[1]
factor n x
| n rem x == 0 =[x] ++ factor n (x-1)
=factor n (x-1)

isPrime :: Int ->Bool
isPrime 1 = False
isPrime n 
| factor n n == [n,1] = True
= False


OnlyPrimePosition::[Int]->[(Int,Int)]
OnlyPrimePosition aList
=[(x,y)\\x<-aList&y<-[1..length aList]|isPrime y]
//Start=OnlyPrimePosition []//[]
//Start=OnlyPrimePosition [1,5,8]//[(5,2),(8,3)]
//Start=OnlyPrimePosition [1..19]//[(2,2),(3,3),(5,5),(7,7),(11,11),(13,13),(17,17),(19,19)]
//Start=OnlyPrimePosition [1,-5,4,3,6,-5,-7,9,-10]//[(-5,2),(4,3),(6,5),(-7,7)]


/*
10.
Write function to calculate n-th Tribonacci number. 
The nth Tribonacci number is defined by the equation:
T(n) = T(n-1) + T(n-2) + T(n-3)
With the starting parameters:
T(0) = 0, T(1) = 0, T(2) = 1
Your solution must be implemented efficiently via
tail recursion.
*/
Thelper :: Int Int Int Int Int-> Int
Thelper t1 t2 t3 0 x =0
Thelper t1 t2 t3 1 x=0
Thelper t1 t2 t3 2 x =1

Thelper t1 t2 t3 n x
| n == x = t1
= Thelper (t1+t2+t3) t1 t2 n (x+1)


T :: Int -> Int
T n
= Thelper 1 0 0 n 3


//Start = T 1 // 0
//Start = T 10 // 44
//Start = T 20 // 19513
//Start = T 50 // 1697490356184
//Start = T 100 // 4130554068881925393


/*
11.
An m-digit Armstrong Number is a number which is equal to sum of digit’s m-th powers.
For example - 153 is a 3 digit Armstrong number: 153 = (1*1*1) + (5*5*5) + (3*3*3).
Write a function which finds the first n Armstrong Numbers
*/
ifArmstrong :: Int Int Int Int -> Bool
ifArmstrong n sum digit remd
| (remd == 0) && ((sum + digit^l) == n) = True
| remd == 0 = False
= ifArmstrong n (sum + digit^l) (remd rem 10) (remd/10)
where l = length [x\\x<-:(toString n)]

armstrong :: Int -> [Int]
armstrong n
= take n [x\\x<-[1..]|ifArmstrong x 0 0 x]

//Start = armstrong 9 // [1,2,3,4,5,6,7,8,9]
//Start = armstrong 15 // [1,2,3,4,5,6,7,8,9,153,370,371,407,1634,8208]
//Start = armstrong 20 // [1,2,3,4,5,6,7,8,9,153,370,371,407,1634,8208,9474,54748,92727,93084,548834]
//Start = armstrong 21 // [1,2,3,4,5,6,7,8,9,153,370,371,407,1634,8208,9474,54748,92727,93084,548834,1741725]


/*
Test Vectors, for your convenience.
a = {x0 = 1, x1 = 2, x2 = 1}
b = {x0 = 3, x1 = 2, x2 = 3}
c = {x0 = 1.0, x1 = 2.0, x2 = 3.0}
d = {x0 = 2.5, x1 = 5.0, x2 = 7.5}
e = {x0 = 4.0, x1 = 5.0, x2 = 6.0}
f = {x0 = 5, x1 = 10, x2 = 5}
*/

/*
12.
Define the record type Vector3 taking type 'a'
and define its instances for +,-,Eq,Ord,Zero.
Ord should be defined as one Vector3 is smaller than
another Vector3 when their distance from the origin
is smaller.
Distance from origin of a vector (x0,x1,x2) can be
calculated by the square root of (x0^2 + x1^2 + x2^2)
Test Vectors and Operations.
a = <1,2,1>
b = <3,2,3>
Zero = <0,0,0>
a + b = <4,4,4>
a - b = <-2,0,-2>
a == b = False
a == a = True
a < b = True
a > b = False
*/
:: Vector3 p = { x :: p, y :: p, z :: p}
instance + (Vector3 a) | + a where + v0 v1 = {x = (v0.x + v1.x), y = (v0.y + v1.y) ,z = (v0.z + v1.z)}
instance - (Vector3 a) | - a where - v0 v1 = {x = (v0.x - v1.x), y = (v0.y - v1.y) , z = (v0.z - v1.z)}
instance == (Vector3 a) | == a where == v0 v1 = (v0.x == v1.x )&&(v0.y == v1.y)&&(v0.z == v1.z)
instance zero (Vector3 a) | zero a where zero  = {x = zero,y = zero , z = zero}
instance < (Vector3 a) | <,+,* a where (<) v0 v1 = (v0.x*v0.x + v0.y*v0.y + v0.z*v0.z ) < (v1.x*v1.x + v1.y*v1.y + v1.z*v1.z)
//instance > (Vector3 a) | >,+,* a where (>) v0 v1 = (v0.x*v0.x + v0.y*v0.y + v0.z*v0.z ) > (v1.x*v1.x + v1.y*v1.y + v1.z*v1.z)
a :: (Vector3 Int) 
a = {x = 1, y = 2, z = 1}
b :: (Vector3 Int) 
b = {x = 3, y = 2, z = 3}
c :: (Vector3 Real) 
c = {x = 1.0, y = 2.0, z = 3.0}
d :: (Vector3 Real) 
d = {x = 2.5, y = 5.0, z = 7.5}
e :: (Vector3 Real) 
e = {x = 4.0, y = 5.0, z = 6.0}
f :: (Vector3 Int) 
f = {x = 5, y = 10, z = 5}
//Start = a == b
//Start = a - b
//Start = a + b
//Start =  a == a
//Start = a < b
//Start = a > b
/*
13.
Using your defined Vector3 record, determine if two Vector3
are linearly dependent.
Two vectors are linearly dependent if multiplying every component
of one vector with a factor will give you the other vector.
For example:
<1, 2, 3> and <2.5, 5, 7.5> are linearly dependent by a factor of
2.5
Test Vectors and Results
<1.0,2.0,3.0> <4.0,5.0,6.0> = False
<1.0,2.0,3.0> <2.5,5.0,7.5> = True
*/


linearDependentHelper :: (Vector3 Real) (Vector3 Real) Real -> Bool
linearDependentHelper v1 v2 c
| ((v1.x / v2.x)== c ) && ((v1.y/v2.y) == c) = True
= False

linearDependent :: (Vector3 Real) (Vector3 Real) -> Bool
linearDependent v1 v2
=linearDependentHelper v1 v2 (v1.z/v2.z)

//Start = linearDependent {x = 1.0 , y = 2.0, z = 3.0} {x = 4.0, y = 5.0,z = 6.0}

/*
14.
One of the most important operations in Ray Tracing is calculating the determinant.
Calculate the determinant for two 3D vectors.
Additional Information:
Your three dimensional vector should be defined above as Vector3.
The determinant of two 3D vectors 'a' and 'b' can be calculated by:
x = (a.y * b.z - a.z * b.y)
y = -1 * (a.x * b.z - a.z * b.x)
z = (a.x * b.y - a.y * b.x)
Test Vectors and Results
<1,2,1> x <3,2,3> = <4,0,-4>A
<3,2,3> x <1,2,1>  = <-4,0,4>
<1,2,1> x <5,10,5> = <0,0,0>
*/
determinant3DVector :: (Vector3 Int) (Vector3 Int) -> (Vector3 Int)

determinant3DVector a b
= {x =(a.y * b.z - a.z * b.y),y =(-1 * (a.x * b.z - a.z * b.x)),z = (a.x * b.y - a.y * b.x)}


//Start = determinant3DVector a b // {4 0 -4}
//Start = determinant3DVector b a // {-4 0 4}
//Start = determinant3DVector a f // { 0 0 0} 

/*
15.
For Ray Tracing, we can record the result as (Red, Green, Blue) or RGB values for every pixel in a file. The file extension is called ppm.
TASK: Create a 6x8 matrix of RGB tuples which holds the values for the flag of Hungary.

Requirement: Use list comprehension.
Hint: Partition the problem into three smaller 2x8 matrices, one for each color, and concatenate them in the end.

Additional Information:
a. RGB values vary from 0 to 255. Use simple color combination to create colors
Ex: RED = (255,0,0)
Ex: GREEN = (0,255,0)
Ex: WHITE = (255,255,255)
b. The Hungarian flag is divided into three equal horizontal rectangles, colored red, white and green, from top to bottom.

Expected result:
[[(255,0,0),(255,0,0),(255,0,0),(255,0,0),(255,0,0),(255,0,0),(255,0,0),(255,0,0)],
 [(255,0,0),(255,0,0),(255,0,0),(255,0,0),(255,0,0),(255,0,0),(255,0,0),(255,0,0)],
 [(255,255,255),(255,255,255),(255,255,255),(255,255,255),(255,255,255),(255,255,255),(255,255,255),(255,255,255)],
 [(255,255,255),(255,255,255),(255,255,255),(255,255,255),(255,255,255),(255,255,255),(255,255,255),(255,255,255)],
 [(0,255,0),(0,255,0),(0,255,0),(0,255,0),(0,255,0),(0,255,0),(0,255,0),(0,255,0)],
 [(0,255,0),(0,255,0),(0,255,0),(0,255,0),(0,255,0),(0,255,0),(0,255,0),(0,255,0)]]
*/


printHungarianFlag :: [[(Int, Int, Int)]]
printHungarianFlag 
= [repeatn 8 x\\x<-flagList]

where flagList = flatten (map (\x = repeatn 2 x) [(225,0,0),(225,225,225),(0,225,0)])
//Start = printHungarianFlag
