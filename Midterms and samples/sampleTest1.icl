module sampleTest1
import StdEnv
import StdOverloaded
import StdList

/**1
  * Write a function, that takes a list of functions, and a list of
  * tuples (Int, Int) where the first Int indicates which function to
  * use and the second Int acts as a parameter and returns a list of
  * the results.
  
  * For example: Router [isEven,isOdd] [(1,2),(2,4),(1,57)] = [True, False, False]
  */
//Solution 1 with using list recursive
Router :: [(a->b)] [(Int,a)] -> [b]
Router [] n = []
Router aList [] = []
Router aList [(a,b):z]
= [(aList!!(a-1)) b : Router aList z]
//Solution 2
Router2 :: [(a->b)] [(Int,a)] -> [b]
Router2 [] listRoute = []
Router2 listFunction [] = []
Router2 listFunction listRoute
= [(listFunction!!(funcNum - 1)) param\\(funcNum,param)<- listRoute]


//Start = [1..5]!!1
//Start = Router [isEven,isOdd] [(1,2),(2,4),(1,57)] //[True, False, False]

//Start = Router [((+)1),((*)2),((^)2),((rem) 100)] [(4,13),(2,23),(3,5),(1,1336),(4,23)] //[9,46,32,1337,8]

//Start = Router [(\x = [1..x]),(\x = [n\\n<-[1..x]|x rem n ==0]),(\x = [x,x*2..x*10])] [(2,36),(1,13),(3,5),(2,128),(3,1)]  //[[1,2,3,4,6,9,12,18,36],[1,2,3,4,5,6,7,8,9,10,11,12,13],[5,10,15,20,25,30,35,40,45,50],[1,2,4,8,16,32,64,128],[1,2,3,4,5,6,7,8,9,10]]

//Start = Router [] [(4,13),(2,23),(3,5),(1,1336),(4,23)] //[]

//Start = Router [isEven,isOdd] [] //[]


/**2
  * Write a function that takes a list of integers and returns a list of
  * result integers based on how many integers were in the parameter list.
  * For 1 integer 'a', it will return that integer modulus 2. (a rem 2)
  * For 2 integers 'a','b' , it will return a list of all integers from the first to the second. [a..b]
  * For 3 integers 'a','b','c' , it will return (a*(b^c))
  * For 4 integers 'a','b','c','d', it will return a list of the sum of 'a' and 'b' and the sum of 'c' and 'd'.
  */

Listing :: [Int] -> [Int]
Listing [] = []
Listing [x] = [x rem 2]
Listing [a,b] = [a..b]
Listing [a,b,c] = [a*(b^c)]
Listing [a,b,c,d] = [a+b,c+d]

//Start = Listing [5] //[1]

//Start = Listing [4,10] //[4,5,6,7,8,9,10]

//Start = Listing [3,5,2] //[75]

//Start = Listing [13,29,1030,307] //[42,1337]

//Start = Listing [] //[]


/**3
  * Write a function that checks if a list of numbers is odd,even,odd,even...
  
  * For exmaple: SeqCheck [1,2,3,4,6] = False because 4 is even, but 6 is not odd.
  */

SeqCheck :: [Int] -> Bool
SeqCheck [] = False
SeqCheck [x] 
|isOdd x = True
=False
SeqCheck [x,y]
|(isOdd x) && (isEven y) =True 
=False
SeqCheck [x,y:z]
| (isOdd x) && (isEven y)==True  = SeqCheck z // attention! isOdd x 
= False


SeqCheck2 :: [Int] -> Bool
SeqCheck2 [] = False
SeqCheck2 aList
=and[isOdd x\\x<-aList & y<-[1..(length aList)]|isOdd y] && and[isEven x\\x<-aList & y<-[1..(length aList)]|isEven y]


SeqCheck3 :: [Int] -> Bool
SeqCheck3 [] = False
SeqCheck3 seq = and[isEven (x+y)\\x<-seq & y<-[1..]]


//Start = SeqCheck3 [1..10] //True

//Start = SeqCheck2 [1,2,3] //True

//Start = SeqCheck2 [2,3,4] //False

//Start = SeqCheck2 [1,3,4,5] //False

//Start = SeqCheck2 [1,2,3,4,6,7] //False

//Start = SeqCheck [] //False



/**4
  * Write a function that checks if each elements in the list appear even times.
  
  * For example, checkEven [1,1,2,2,2,2,3,5,3,5] = True
  */
checkEven :: [Int] -> Bool
checkEven []=False
checkEven [x] = False
checkEven [x,y] 
| x==y = True
=False 
checkEven aList
| x==y =checkEven z
= False 
where [x,y:z] = sort aList


checkEven2 :: [Int] ->Bool
checkEven2 [] = False
checkEven2 aList
=and [isEven(length (filter ((==)x) aList))\\x<-aList]

//Start = checkEven2 [1,1,2,2,2,2,3,5,3,5] // True

//Start = checkEven [1,1,2,2,1] // False

//Start = checkEven [] //False

/**5
  * Write a function that takes two vectors, represented as lists, and returns their dot product.
  
  * The dot product of two vectors can be computed as:
  
  * < xa, xb, xc, ...> * < ya, yb, yc, ...> = (xa*ya) + (xb*yb) + (xc*yc) + ...
  
  * For example: DotProd [4,6,3] [6,3,7] = 24+18+21 = 63
  */

DotProd :: [Int] [Int] -> Int
DotProd x [] =0
DotProd [] y =0
DotProd [x] [y] = x*y
DotProd [x:y] [m:n]
= x*m + DotProd y n

DotProd2 :: [Int] [Int] -> Int
DotProd2 aList1 aList2
=sum[x*y\\x<-aList1&y<-aList2]

//Start = DotProd2 [4,6,3] [6,3,7] //63

//Start = DotProd [6,3,7] [4,6,3] //63

//Start = DotProd [5,2,6,8,3] [5,-8,5,-3,-5] //0



// Given a list of characters, split it into a tuple in which the first part only contains digits ('0'..'9'),
// the second part contains the rest. 


IsDigit :: Char -> Bool 
IsDigit x
| ((toInt x) >=49) &&((toInt x) <=57) =True
=False

TwoLists :: [Char] -> ([Char], [Char])
TwoLists [] = ([],[])
TwoLists aList 
=([x\\x<-aList |IsDigit x],[x\\x<-aList| (IsDigit x)==False ])
//Start = toInt '9'
// 'a' 97 - 122  'A' 65 -90 '1' = 49  57

TwoLists2 :: [Char] -> ([Char], [Char])
TwoLists2 aList
=(filter (\char = char >='0' && char <='9') aList,filter (\char = char>='a'&&char <= 'z') aList)
//Start = TwoLists2  ['1', 'a', '2', 'b', '3'] // (['1','2','3'],['a','b'])
//Start = TwoLists [] // ([],[])

//Given a list of lists, for each list, extract the first, middle and last element. */

// first : hd middle !!(length aList)/2  last 
Points3helper ::[Int] -> (Int,Int,Int)
Points3helper aList
=(hd aList, aList!!mid, last aList)
where mid = (length aList)/2

Points3 :: [[Int]] -> [(Int, Int, Int)]
Points3 [[]]=[]
Points3 n
= map Points3helper n 

Points4 :: [[Int]] -> [(Int, Int, Int)]
Points4 aList
=[(hd x,x!!((length x)/2),last x )\\x<- aList] 

//Start = Points4 [[1..9], [2..6], [3..11], [1..10]] // [(1,5,9),(2,4,6),(3,7,11),(1,6,10)]

//Start = Points3 [[]] //[]



//Find the 'unique' right triangle in the list eg. (3,4,5) and (4,3,5) are the same triangle. 
//only one will appear in the answer list [(3,4,5),(4,3,5)] -> [(3,4,5)] */

sortTuple :: (Int,Int,Int) -> (Int,Int,Int)
sortTuple (a,b,c)
=(r!!0 , r!!1 , r!!2)
where r= sort[a,b,c]

sortTupleList :: [(Int,Int,Int)] -> [(Int,Int,Int)]
sortTupleList aList
=[sortTuple x\\ x <- aList ]


select :: [(Int,Int,Int)] ->[(Int,Int,Int)]
select [] = []
select [(a,b,c):y]
|(a rem 3 ==0) && (b rem 4 == 0) && (c rem 5 == 0) && (a/3 ==b/4)&&(a/3 == c/5) = [(a,b,c):select y]
= select y 

reduRedu :: [(Int,Int,Int)] ->[(Int,Int,Int)]
reduRedu [] = []
reduRedu [(0,0,0)]=[]
reduRedu [x:y]
=[x:reduRedu(filter ((<>)x) y)]

f8::[(Int,Int,Int)]->[(Int,Int,Int)]
f8 aList
=reduRedu(select(sortTupleList aList))


//Start = f8 [(3,4,5),(4,5,6),(4,5,3),(6,8,10),(10,5,8),(-3,4,5)] //[(3,4,5),(6,8,10)]

//Start = f8 [(1,1,1),(5,4,3),(3,4,5),(0,0,0)] //[(5,4,3)]



//Use foldr to check if the square root of each integer in a list are all integers. 
squareInt :: Int Int -> Bool
squareInt 1 1 =True
squareInt n 1 =False
squareInt n s
| s*s == n =True
=squareInt n (s-1)

f9::[Int] ->Bool
f9 [] = True
f9 aList
= foldr (\x y = (squareInt x x) && y  ) True aList


//Start = f9 [] //True

//Start = f9 [4,16,9] //True

//Start = f9 [1,8] //False

 
//10 Insert sum of elements as last element in every sublist of a list. 
//Solution 1
sumofList:: [Int] -> [Int]
sumofList []= [0]
sumofList x = x++[sum x]

addSum :: [[Int]] -> [[Int]]
addSum n
= map sumofList n
//Solution2
addSum2 :: [[Int]] -> [[Int]]
addSum2 n
=[ (aList ++ [sum aList]) \\ aList <- n]
//Start = addSum2 [[1,2], [3,4,5], [6,5,9,7], [], [8]] //[[1,2,3],[3,4,5,12],[6,5,9,7,27],[0],[8,8]]











