module hw04
import StdEnv
import StdOverloaded
/**
  * 30 pts
  * Write a function that takes a list of integers
  * and returns the variance of the list.
  * That is, the sum of the square differences from the mean divided by
  * number of elements - 1.
  * For example, variance of [1,2,3,4,5] is calculated by:
  * Mean = (1+2+3+4+5)/5 = 3
  * Sum of Square Differences = (1-3)^2 + (2-3)^2 + (3-3)^2 + (4-3)^2 + (5-3)^2 = 10
  * Variance = 10/(5-1) = 10/4 = 2.5
  *
  * Note: Your solution must use 'map' or a list comprehension.
  */
  
mean :: [Int] Int Int -> Real
mean [] sum n = toReal(0)
mean [x:y] sum n
| isEmpty y =  (toReal(sum+x)/toReal(n+1))
=mean y (sum+x) (n+1)

squareSum :: [Int] Real  -> Real
squareSum aList m
=sum[(toReal(x)-m)^toReal(2)\\x<-aList]

Variance :: [Int] -> Real
Variance aList 
= squareSum aList (mean aList 0 0)/toReal(length(aList)-1)

//Start = Variance [1..5] //2.5
//Start = Variance [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1] //0
//Start = Variance [-4,1,6,0,-2,6] //16.96666666666
//Start = Variance [] //0

/**
  * 30 pts
  * Write a function that takes a list of integers and reverses their digits and order.
  * 
  * For example: ReverseDig [123,456,789] = [987,654,321]
  */
  
//Way 1
fromIntToReverseInt :: Int ->Int
fromIntToReverseInt n = toInt(toString(reverse [el \\ el<-:(toString (n))] ))
  
ReverseDig :: [Int] -> [Int]
ReverseDig aList
=reverse [fromIntToReverseInt n \\ n <-aList]


//Way 2
digitReverse :: Int Int ->Int
digitReverse 0 r =r
digitReverse n r
= digitReverse (n/10) (r*10+(n rem 10))

ReverseDig2 :: [Int] -> [Int]
ReverseDig2 aList
=reverse [digitReverse n 0 \\ n <-aList]

//Start = ReverseDig2 [123,456,789] //[987,654,321]
//Start = ReverseDig [] //[]
//Start = ReverseDig [1,23,456,7891,23456] //[65432,1987,654,32,1]


/**
  * 40 pts
  * Write a function that takes a predicate (a -> Bool function) and
  * a list of sublists of integers and returns the sum of all elements that
  * return True on both  or one of the two predicates depending on
  * the given parameter "or"/"and".
  */
 
//WAY 1
intersection :: [Int] [Int] -> [Int]
intersection [] aList =[]
intersection [x:y] aList
| isMember x aList =[x:intersection y aList]
=intersection y aList

union :: [Int][Int]->[Int]
union [] aList =aList
union [x:y] aList
| isMember x aList =union y aList
=[x:union y aList]

FilterSum :: (Int -> Bool) String (Int -> Bool) [[Int]] -> Int
FilterSum f1 s f2 aList
| s == "or" =sum(union [x\\x<-(flatten aList)|f1 x] [x\\x<-(flatten aList)|f2 x])
=sum (intersection [x\\x<-(flatten aList)|f1 x] [x\\x<-(flatten aList)|f2 x])
//WAY2
FilterSum2 :: (Int -> Bool) String (Int -> Bool) [[Int]] -> Int
FilterSum2 f1 s f2 aList
| s == "or" =sum([x\\x<-(flatten aList)|(f1 x)||(f2 x)])
=sum ([x\\x<-(flatten aList)|(f1 x) && (f2 x)] )


Start = FilterSum isEven "or" ((<) 3) [[1..5],[-2..10]] //60
//Start = FilterSum ((<)10) "and" (\x=isEmpty[div\\div<-[2..(x-1)]|x rem  div == 0]) [[1..20],[90..100],[1..10]] //157
//Start = FilterSum isOdd "or" isEven []//0

//Start = FilterSum2 isEven "or" ((<) 3) [[1..5],[-2..10]] //60
//Start = FilterSum2 ((<)10) "and" (\x=isEmpty[div\\div<-[2..(x-1)]|x rem  div == 0]) [[1..20],[90..100],[1..10]] //157
//Start = FilterSum2 isOdd "or" isEven []//0

























