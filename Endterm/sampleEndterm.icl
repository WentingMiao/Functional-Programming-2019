module sampleEndterm
import StdEnv

// Solve as many functions as you can. Each exercise is of 10%, to pass min. 40% is necessary.
// marks: 40%-2,60%-3,80%-4,100%-5. 
//1.
//Define a tree type and use the followings for testing your solution.
:: Tree a = Node a (Tree a) (Tree a) | Leaf

tree1 = Node 10 (Node 7 (Node 3 Leaf Leaf) (Node 15 Leaf Leaf)) (Node 5 Leaf (Node 10 Leaf Leaf))
tree2 = Node 9 (Node 1 (Node 0 (Node 7 Leaf Leaf) Leaf) (Node 15 Leaf Leaf)) (Node 4 (Node 4561 Leaf Leaf) (Node 8 (Node 1663 Leaf Leaf) Leaf))
//Start = tree1
unitTree = Node 1337 Leaf Leaf
noTree = Leaf



//Write a function that takes a tree as a parameter and returns a list of nodes which have at least one prime child.
//An empty tree will return [].


/*
isPrime :: Int -> Bool
isPrime 0 = False 
isPrime x = and[x rem n <> 0\\n <- [2..(x-1)]]
*/
toLeft :: (Tree a) ->(Tree a)
toLeft (Node x l r) = l

toRight :: (Tree a) ->(Tree a)
toRight (Node x l r) = r

subTreeList :: (Tree a) -> [(Tree a)]
subTreeList Leaf = []
subTreeList tree
= (subTreeList (toLeft tree))++ [tree]++(subTreeList (toRight tree))


getChildren :: (Tree a) -> [a]
getChildren Leaf = []
getChildren (Node x Leaf Leaf)=[]
getChildren (Node x l Leaf ) =[getNode l]
getChildren (Node x Leaf  r ) =[getNode r]
getChildren (Node x l r) 
= [getNode l,getNode r]

primeChildren :: (Tree Int) -> [Int]
primeChildren atree
= [ getNode subtree\\subtree <- (subTreeList atree)| length [x\\x<- (getChildren subtree)|isPrime x ] >0 ]


//solution2
getNode2 :: (Tree Int) -> Int
getNode2 Leaf = 0
getNode2 (Node x l r) =x
primeChildren2 :: (Tree Int) ->[Int]
primeChildren2 Leaf =[]
primeChildren2 (Node x l r)
| isPrime  (getNode2 l) = [x] ++ (primeChildren2 l)++(primeChildren2 r)
| isPrime (getNode2 r)= [x]++(primeChildren2 l)++(primeChildren2 r)
=(primeChildren2 l)++(primeChildren2 l)

//Start = primeChildren2 tree1 //[10,7]
//Start = primeChildren tree2 //[0,4,8]
//Start = primeChildren unitTree //[]
//Start = primeChildren noTree //[]

//2.
//Given a tuple of arrays, representing sets of integers, return a list containing the result of their symmetric-difference.
//The symmetric-difference between two sets is equivalent to the difference between their union and their intersection.
/* 
//inefficient solution First
union :: [Int] [Int] -> [Int]
union aList bList
= removeDup (aList ++ bList)

intersection :: [Int] [Int] ->[Int]
intersection [] aList = []
intersection [x:y] aList
| isMember x aList = [x] ++ intersection y aList
= intersection y aList

//Start = intersection [1,2,5] [1,5,8]

symmetricDiff :: ({Int}, {Int}) -> [Int]
symmetricDiff (aArray,bArray) 
= diff (union aList bList)   (intersection aList bList)
where
 	aList = [x\\x<-:aArray]
 	bList=[y\\y<-:bArray] 

diff :: [Int] [Int] -> [Int]
diff big [] = big
diff big [x:y]
|isMember x big = diff (filter ((<>)x) big) y
= diff big y
*/
//second Solution 
symmetricDiff :: ({Int}, {Int}) -> [Int]
symmetricDiff (aArray,bArray) 
= [y\\y<-:bArray | isMember y [x\\x<-:aArray] == False]++[y\\y<-:aArray | isMember y [x\\x<-:bArray] == False]
//Start = symmetricDiff2 ({1,2,3,4},{3,4,5,6}) //[1,2,5,6]
//Start = symmetricDiff ({1,2,3,4},{-2,-4,13,0}) //[1,2,3,4,-2,-4,13,0]
//Start = symmetricDiff ({1,2,3,4},{1,2,3,4}) //[]
//Start = symmetricDiff ({1,2,3,4},{}) //[1,2,3,4]
//Start = symmetricDiff ({},{1,2,3,4}) //[1,2,3,4]
//Start = symmetricDiff ({},{}) //[]

//3.
//Define a Q type for rational numbers.
//Write a function that receives two fractions and calculates their division. Simplify the fraction before returning.
//In case the nominator is zero, set the denominator to zero as well.
:: Q = {nom :: Int, den::Int}
fracDivision :: Q Q -> Q
fracDivision {nom = a,den = b} {nom =c,den = d}
= simplify {nom = a*d, den = b*c}

simplify :: Q->Q
simplify {nom = a,den = b}
|( a == 0) = abort "Error"
= {nom = a/max,den = b/max}
where
	max = gcd a b
//Start = simplify {nom = 0,den =5}
//Start = fracDivision {nom=5, den=1} {nom=6, den=5} //{nom=25, den=6}
//Start = fracDivision {nom=10, den=2} {nom=3, den=4} //{nom=20, den=3}
//Start = fracDivision {nom=0, den=2} {nom=100, den=4} //{nom=0, den=0}
//Start = fracDivision {nom=15, den=2} {nom=3, den=12} //{nom=30, den=1}

half = { nom=1, den=2 }
third = { nom=1, den=3 }
fourth = { nom=1, den=4 }
fifth = { nom=1, den=5 }
sixth = { nom=1, den=6 }
threehalf = { nom=3, den=2 }
twothird = { nom=2, den=3 }
ninefourth = { nom=9, den=4 }
threefifth = { nom=3, den=5 }

miniTree = Node fifth (Node sixth Leaf Leaf)(Node third (Node fourth Leaf Leaf) Leaf)			
smallTree = Node half (Node fourth Leaf Leaf) (Node ninefourth Leaf Leaf)
bigTree = Node half (Node fifth (Node sixth Leaf Leaf)(Node third (Node fourth Leaf Leaf) Leaf))(Node threehalf (Node threefifth Leaf (Node twothird Leaf Leaf))(Node ninefourth Leaf Leaf))
badTree = Node third (Node fourth Leaf Leaf)(Node ninefourth (Node sixth Leaf Leaf) Leaf)

//4.
//Write a function that will return the sum of a tree's nodes.
//Return the sum as a simplified Q.
treeToList :: (Tree a) -> [a]
treeToList Leaf = []
treeToList (Node x l r)
= treeToList l ++[x]++ treeToList r

add :: Q Q-> Q
add {nom = a,den = b} {nom =c,den = d}
| (a==0) ={nom =c,den = d}
| (c==0) ={nom = a,den = b}
= simplify {nom = a*d+b*c , den = b*d}

zero = {nom = 0,den = 0} 

//Start = treeToList smallTree

sumTree :: (Tree Q) -> Q
sumTree tree
= foldr add zero (treeToList tree) 
//Start = sumTree smallTree //{nom=3,den=1}
//Start = sumTree bigTree //{nom=97,den=15}
//Start = sumTree miniTree //{nom=19, den=20}

//5.
//Write a function that will check if a tree of Q is a Binary Search Tree.

//First Solution
instance < Q 
where
	(<) {nom = a,den = b} {nom =c,den = d}
	| a==0 && c==0 = False
	|a==0 =False
	|b==0 =True
	 = (a*d<c*b)
	 
instance == Q
where
	(==) {nom = a,den = b} {nom =c,den = d}= (a==c)&&(b==d)
//Start = fifth < fourth
//Start= zero < {nom = 0,den = 5}


checkTree2 :: (Tree a) -> Bool| Ord,Eq a
checkTree2 aTree
=  (treeToList aTree) == (sort (treeToList aTree))

//Start = checkTree2 bigTree
//Start = checkTree2 badTree //False
//Start = checkTree2 smallTree //True

//Second solution
getNode ::(Tree n) ->n
getNode (Node x l r) = x

isLeaf ::(Tree n) -> Bool
isLeaf Leaf = True
isLeaf _ = False

checkTree :: (Tree Q) -> Bool
checkTree (Node x l r)
|isLeaf l && isLeaf r = True
|isLeaf l =  x<(getNode r)&&checkTree r
|isLeaf r =  x>(getNode l)&&checkTree l
= (x < getNode r) && (x > (getNode l)) && (checkTree l) &&(checkTree r)

//Start = checkTree bigTree //True
//Start = checkTree smallTree //True
//Start = checkTree badTree //False
:: Color = Red | Yellow | Green | Blue | Purple | Violet
:: ColorCombo = { color1 :: Color, color2 :: Color}

//6.
//Write a function that when given a color, returns its complement.
//That is:
//Red -> Blue, Yellow -> Purple, Green -> Violet, Blue -> Red, Purple -> Yellow, Violet -> Green
colorComp :: Color -> Color
colorComp Red = Blue
colorComp Blue = Red
colorComp Green =Violet
colorComp Purple = Yellow
colorComp Violet = Green
//Start = colorComp Red //Blue
//Start = colorComp Blue //Red
//Start = colorComp Green //Violet
//Start = colorComp Purple //Yellow

//7.
//Write a function that when given a Color, creates a list of possible color combos.
//Valid color combos can not have duplicate colors.
courlist = [Red,Blue,Yellow,Green,Purple,Violet]
instance == Color 
where
	(==) Red Red = True
	(==) Blue Blue = True
	(==) Yellow Yellow = True
	(==) Green Green = True
	(==) Purple Purple = True
	(==) Violet Violet = True
	(==) _ _ = False
colorCombo :: Color -> [ColorCombo]
colorCombo x
=[{color1= x,color2= y}\\ y<- removeMember x courlist]
//Start = colorCombo Red //[{color1=Red, color2=Yellow},{color1=Red, color2=Green},{color1=Red, color2=Blue},{color1=Red, color2=Purple},{color1=Red, color2=Violet}]
//Start = colorCombo Blue //[{color1=Blue, color2=Red},{color1=Blue, color2=Yellow},{color1=Blue, color2=Green},{color1=Blue, color2=Purple},{color1=Blue, color2=Violet}]

//8.
//Amicable numbers are two different numbers so related that the sum of the proper divisors of each 
//is equal to the other number. (A proper divisor of a number is a positive factor of that number other than the number itself. 
//For example, the proper divisors of 6 are 1, 2, and 3.) 
//Check if two integers are amicable pairs and put them together with the answer in a bag.
//amicable pair: 1184 and 1210 
//proper divisor of 1184 :  1, 2, 4, 8, 16, 32, 37, 74, 148, 296, 592 -> their sum == 1210
//proper divisor of 1210 : 1, 2, 5, 10, 11, 22, 55, 110, 121, 242, 605 -> their sum == 1184
factor :: Int Int -> [Int]
factor n 1 = [1]
factor n x
| n rem x == 0 = [x:factor n (x-1) ]
=factor n (x-1)

properdiv :: Int -> [Int]
properdiv n = factor n (n-1)
//Start = properdiv 1184 

isAmi :: Int Int -> Bool
isAmi a b = ((sum (properdiv a)) == b)&&((sum (properdiv b)==a))

//Start = isAmi 1184 1210
:: Bag a :==[((Int,Int),Bool)]

amiBag :: [(Int,Int)] -> Bag a
amiBag [] =[]
amiBag [(a,b):rest]
= [((a,b),(isAmi a b))]++ amiBag rest
//Start = amiBag [(1184,1210), (13,245)]
//Start = amiBag [] // []
//all true
//Start = amiBag [(220, 284), (1184, 1210), (2620, 2924), (5020, 5564), (6232, 6368), (10744, 10856), (12285, 14595), (17296, 18416), (63020, 76084), (66928, 66992)]

//9.
//Given the Object type, compute for the state component the given method and print the result as a String.
//ex: for state 3 compute 3 + 1 using the given method, and print the result "4" as string.
:: Object = {state::Int, method::Int->Int, tostring:: Int -> String }
MyObject = { state = 3, method = (+) 1, tostring = toString}
OjectFun :: Object -> String
OjectFun {state= x, method=f, tostring=_ }
= toString (f x )

//Start = OjectFun MyObject
//10.
//Given an operator and two lists, apply the operator to the elements of 
//the same positions of lists, like in the examples.
:: Operator a :== a a -> a
op2 :: (Operator a) [a] [a] -> [a]
op2 op aList bList
= [op x y\\x<-aList&y<-bList]






//Start = op2 (*) [2,3,4,5] [7,8,9,10] // [14,24,36,50]
//Start = op2 (*) [2,3,4,5] [7,8] // [14,24]
//Start = op2 (*) [2,3] [7,8,9,10] // [14,24]
//Start :: [Int]
//Start = op2 (*) [] [] // []
//Start = op2 (+) [3,2] [7,8] // [10,10]
isPrime n= and[n rem x <>0\\x<-[2..(n-1)]]

//Start = take 100 [x\\x<-[1..]|isPrime x]



















