module treeDefine
import StdEnv

//Define a tree

:: Tree a = Node a (Tree a) (Tree a) | Leaf

ourTree :: (Tree Int)
ourTree = Node 15 (Node 3 (Node 1 Leaf Leaf) (Node 10 (Node 7 Leaf (Node 8 (Node 1 Leaf Leaf) Leaf)) (Node 13 (Node 11 Leaf Leaf) Leaf))) (Node 20 (Node 18 Leaf (Node 19 Leaf Leaf)) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))
//Start = ourTree

messyTree :: (Tree Int)
messyTree = Node 5(Node 12(Node 8 Leaf (Node 1 Leaf Leaf))(Node 6 (Node 9 Leaf Leaf) Leaf))(Node 2 (Node 3 Leaf (Node 13(Node 100 Leaf Leaf)(Node 21 Leaf Leaf)))(Node 40 (Node 60 (Node 70 (ourTree) Leaf) Leaf) Leaf))
//Start = messyTree


////Getting the value at the node
getNode :: (Tree a) -> a
getNode (Node x l r) = x

//Start = getNode ourTree

//Go to the left of the tree

toLeft :: (Tree a) -> (Tree a)
toLeft (Node x l r) = l
toRight ::(Tree a) -> (Tree a)
toRight (Node x l r) = r

//Checking if we're at a leaf
isLeaf :: (Tree a ) -> Bool
isLeaf Leaf = True
isLeaf _ = False


//Get a list of subtrees from a node.
getSubtree :: (Tree a ) -> [(Tree a)]
getSubtree (Node x l r) = [l,r]

//get a minimum value of a tree
getMin :: (Tree a) -> a
getMin (Node x l r) 
| isLeaf l = x
= getMin l

//Reverse a tree
reverseTree :: (Tree a) -> (Tree a)
reverseTree Leaf = Leaf
reverseTree (Node x l r)
= Node x (reverseTree r) (reverseTree l)

//Start = reverseTree ourTree

//get the maximum value of a Tree
getMax :: (Tree a) -> a
getMax (Node x l r)
| isLeaf r = x
= getMax r

getMax2 :: (Tree a) -> a
getMax2 tree
= getMin (reverseTree tree)

//Start = getMax2 ourTree

//Create a list of all Node in order
treeTolist :: (Tree a) ->[a]
treeTolist Leaf = []
treeTolist (Node x l r)
= (treeTolist l) ++ [x] ++(treeTolist r)

//Start = treeTolist ourTree


//Create a list of subtree 
subTreeList :: (Tree a) ->[(Tree a)]
subTreeList Leaf = []
subTreeList tree
= subTreeList (toLeft tree) ++ [tree] ++ subTreeList (toRight tree)

//Start = subTreeList ourTree


// get the subtree witch has the specific element 
getsomeSubtree :: a (Tree a) -> [(Tree a)] |Eq a
getsomeSubtree n atree
= [x\\x<-(subTreeList atree)|(getNode x) == n] 


//Start = getsomeSubtree 20 ourTree

//Get a list of children of a node
getChildren :: a (Tree a) ->[a] |Eq a
getChildren n aTree
| isLeaf (toLeft subTree) && isLeaf (toRight subTree) =[]
| isLeaf (toLeft subTree) = [getNode (toRight subTree)]
| isLeaf (toRight subTree) = [getNode (toLeft subTree)]
= [getNode (toLeft subTree)]++ [getNode (toRight subTree)]
where subTree = hd (getsomeSubtree n aTree)

//Start = getChildren 20 ourTree

//Get the parent of a node
findParent :: a (Tree a) ->[a]| Eq a
findParent n Leaf = []
findParent n aTree
| n == getNode aTree = []
| isMember n (getChildren (getNode aTree) aTree) = [getNode aTree]
= findParent n (toLeft aTree)++  findParent n (toRight aTree)


//Add a new node to a BST
addNode :: a (Tree a) -> (Tree a)| Ord ,Eq a
addNode n Leaf = (Node n Leaf Leaf)
addNode n (Node x l r)
| x == n = (Node x l r)
| n < x = (Node x (addNode n l) r)
| n > x = (Node x l (addNode n r))



//Start = addNode 20 ourTree

//Remove the minimum node of a binary search tree

remvMin :: (Tree a) -> (Tree a)| Eq a
remvMin (Node x Leaf r) = r
remvMin (Node x l r) 
| getNode l == getMin (Node x l r)  = (Node x (toRight l) r)
= (Node x (remvMin l) r)


//remove the root of a BST
remRoot :: (Tree a) ->(Tree a)| Eq a
remRoot (Node x Leaf Leaf)= Leaf
remRoot (Node x Leaf r) = r
remRoot (Node x l Leaf ) = l 
remRoot (Node x l r)
= (Node (getMin r) l (remvMin r))



//Start = remRoot ourTree


//Filter a binary search tree
filterTree ::  (a -> Bool) (Tree a) ->(Tree a)|Eq a
filterTree con Leaf = Leaf
filterTree con (Node x l r)
| con x = (Node x (filterTree con l) (filterTree con r)) 
= remRoot (Node x (filterTree con l) (filterTree con r)) 

//Start = filterTree ((>)8) ourTree


//Make a BST from a list
listToTree :: [a] ->(Tree a)| Ord,Eq a
listToTree aList
= foldr addNode Leaf aList


//Start = listToTree [1,2,5,9,6,6,44,52]


//Make a balanced BST from a list

balancedTree :: [a] -> (Tree a)|Eq ,Ord a
balancedTree aList
= foldr addNode rootTree aList
where 
	root = sort (removeDup aList) !! ((length (removeDup aList) )/2)
	rootTree = (Node root Leaf Leaf)
	
	












