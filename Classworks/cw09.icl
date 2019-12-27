module cw09
import StdEnv

/*
1. Check whether a binary search tree is a degenerate case
(
  Degenerate tree: All of its nodes (for simplicity, exclude leaves) formed a straight line
  Binary search tree: For every nodes, its left node value < its value < its right node value.
)
*/


:: Tree a = Node a (Tree a) (Tree a) | Leaf 


treeTolist :: (Tree a) -> [a]
treeTolist Leaf = []
treeTolist (Node x l r) = (treeTolist l) ++ [x]++ (treeTolist r)

inorder :: [a] -> Bool| < a
inorder [a] = True
inorder [a,b] 
| a< b = True
=False
inorder [a,b:c]
| a<b = inorder [b:c]
=False

f1 :: (Tree a) -> Bool| < a
f1 tree
= inorder (treeTolist tree)


//Start = f1 (Node 3 (Node 2 (Node 1 Leaf Leaf) Leaf) Leaf) // True
//Start = f1 (Node 1 Leaf (Node 2 Leaf (Node 3 Leaf Leaf))) // True
//Start = f1 (Node 1 Leaf (Node 2 Leaf (Node 1 Leaf Leaf))) // False
//Start = f1 (Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)) // False




//2. Define rational class Q for rational numbers. Define instances for addition and multiplication
:: Q  = { nom :: Int, den::Int}
Qzero ={nom = 0,den = 1}

simplify :: Q -> Q
simplify {nom = n,den = d} 
| d == 0 = Qzero
| d < 0 = { nom = ~n/g, den = ~d/g} 
| otherwise = { nom = n/g, den = d/g} 
where g = gcd n d

instance + Q 
where 
	(+) x y = simplify{nom= (x.nom*y.den+y.nom*x.den),den= (x.den*y.den)}


instance * Q
where 
	(*) x y = simplify {nom = (x.nom*y.nom) ,den = (y.den*x.den)}

//Start = {nom=2, den=4} + {nom=5, den=6} // (Q 4 3)
//Start = {nom=2, den=4} * {nom=5, den=6}  // (Q 5 12)


//3. Define abstract type PriorityQueue

:: Queue a :== [a]

newQueue :: (Queue a) // Creates empty queue 
newQueue = []

isempty :: (Queue a) -> Bool // Checks if a queue is empty 
isempty [] =True
isempty _ = False


push :: a (Queue a) -> Queue a // add an item to the end of the queue 
push elem alist
= alist ++ [elem]

pop :: (Queue a) -> (Queue a, a) | Ord a // Remove the biggest item from the queue 
pop [x:y] = ([x:y], biggest)
where biggest = last (sort [x:y])



PQ :: (Queue Int)
PQ = [1, 2, 3, 4, 5]


//Start = isempty newQueue //True
//Start =  push 1 PQ //[1,2,3,4,5,1]
Start =  pop PQ //([1,2,3,4], 5)









