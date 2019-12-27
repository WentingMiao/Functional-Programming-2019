module cw10
import StdEnv

:: Tree a = Node a (Tree a) (Tree a) | Leaf

ourTree :: (Tree Int)
ourTree = Node 15(Node 3(Node 1 Leaf Leaf)(Node 10(Node 7 Leaf (Node 8 Leaf Leaf))(Node 13 (Node 11 Leaf Leaf) Leaf)))(Node 20 (Node 18 Leaf (Node 19 Leaf Leaf)) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))
tree1 = Node 10 (Node 7 (Node 3 Leaf Leaf) (Node 15 Leaf Leaf)) (Node 5 Leaf (Node 10 Leaf Leaf))
tree2 = Node 9 (Node 1 (Node 0 (Node 7 Leaf Leaf) Leaf) (Node 15 Leaf Leaf)) (Node 4 (Node 4561 Leaf Leaf) (Node 8 (Node 1663 Leaf Leaf) Leaf))
unitTree = Node 1337 Leaf Leaf
noTree = Leaf


treeTolist :: (Tree a) ->[a]
treeTolist Leaf = []
treeTolist (Node x l r)
= (treeTolist l) ++ [x] ++(treeTolist r)


/*
Write a function returning the depth
of the largest prime number in the tree.
*/
isPrime :: Int -> Bool
isPrime x = and[x rem n <> 0\\n <- [2..(x-1)]]

treeTolistPrime :: (Tree Int) Int ->[(Int,Int)]
treeTolistPrime Leaf depth = []
treeTolistPrime (Node x l r) depth
= (treeTolistPrime l (depth+1)) ++ [(x,depth)] ++(treeTolistPrime r (depth+1))

searchDeep :: [(Int,Int)] Int -> Int
searchDeep [] max = 999
searchDeep [(x,d):rest] max
| x == max = d
= searchDeep rest max

deepPrime :: (Tree Int) -> Int
deepPrime Leaf = 0
deepPrime aTree
= searchDeep (treeTolistPrime aTree 0) max
where max = last (sort [x\\x<-treeTolist aTree|isPrime x])

//Start = last (sort [x\\x<-treeTolist ourTree|isPrime x])
//Start = deepPrime ourTree //3
//Start = deepPrime tree1 //1
//Start = deepPrime Leaf //0

/*
Write a class 'lol'
with instances for Int, Real, Bool, String, and [a].
For Int, return that number + 1.
For Real, return the square root of that number.
For Bool, return the opposite boolean.
For String, return the String in reverse.
For a list [a], return the list concatenated to itself.
*/

class lol a :: a -> a

instance lol Int
where
	lol :: Int -> Int
	lol x = x+1
	
	
instance lol Real
where 
	lol :: Real -> Real
	lol x = sqrt x
	
instance lol Bool
where
	lol :: Bool ->Bool
	lol True = False
	lol False = True
	
instance lol String
where 
	lol :: String -> String
	lol s = toString (reverse [x\\x<-:s])

instance lol [a]
where
	lol :: [a] ->[a]
	lol a = a ++ a
//Start = lol 41 //42
//Start = lol 176752.9764 //420.42
//Start = lol True //False
//Start = lol "partyboob" //"boobytrap"
//Start = lol [1,2,3,4] //[1,2,3,4,1,2,3,4]

/*
Given an array, extract the elements
located at the prime numbered index locations
and return them in a new array.
*/

primeIndex :: (a e) -> (a e) | Array a e

primeIndex myArray
= {x\\x<-:myArray&y<-[1..]|isPrime y}


//Start :: {Int}
//Start = primeIndex {x\\x<-[1..100]}
//{1,2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97}
//Start = primeIndex "Helplooblo aWes omrporlyeet!d !"