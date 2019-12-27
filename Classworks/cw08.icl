module cw08
import StdEnv

/*
Given an array of Int and a single Int, use array
comprehension to double each element of the array,
keeping only the multiples of the second Int argument.
*/

f1 :: {Int} Int -> {Int}
f1 aList n
={x*2 \\ x<-:aList|((x*2) rem n == 0)}
//Start = f1 {1,2,3,4} 4 //{4,8}
//Start = f1 {3,4,5,7,2,9} 3 //{6,18}

/*
Given a Tree with nodes of type Person,
return the number of people who are older than 18.
That is, people born on or before 2001.11.22
*/
::Person = { name::String
			,birthday::(Int,Int,Int)
	}
::Tree a = Node a (Tree a) (Tree a)
	|Leaf

t1::Tree Person
t1 = Node {name = "hh", birthday = (2001,11,22)} Leaf Leaf
t2::Tree Person
t2 = Node {name = "hh", birthday = (2001,11,22)} (Node {name = "hr", birthday = (2001,11,21)} Leaf Leaf)(Node {name = "ht", birthday = (2001,11,23)} Leaf Leaf)
t3::Tree Person
t3 = Node {name = "hh", birthday = (2001,11,22)} (Node {name = "hr", birthday = (2001,11,21)} (Node {name = "hh", birthday = (2002,11,22)} Leaf Leaf) (Node {name = "hh", birthday = (1998,11,22)} Leaf Leaf))(Node {name = "ht", birthday = (2001,11,23)} Leaf Leaf)


treetoList :: (Tree a) -> [a]
treetoList Leaf =[]
treetoList (Node n l r) = treetoList l++[n]++ treetoList r

isOlder :: (Int,Int,Int) (Int,Int,Int) ->Bool
isOlder (py,pm,pd) (ey,em,ed)
| py < ey = True
| (py == ey) && (pm < em) = True
| (py == ey) && (pm == em) && (pd <= ed) = True
=False


f2 :: (Tree Person) -> Int
f2 treePerson
=length[x\\x<-(treetoList treePerson)|isOlder x.birthday (2001,11,22)]



//Start = f2 t2 //2
//Start = f2 t3  //3

/*
Given a Tree of type Person, return the same tree, except
with "_qualify" attached to the end of the names of each person
who is over 18.
*/

f3 :: (Tree Person) ->(Tree Person)
f3 Leaf = Leaf
f3 (Node x l r)
|isOlder (x.birthday) (2001,11,22) = (Node {x &name = (x.name +++ "_qualify")}) (f3 l) (f3 r)
=Node x (f3 l) (f3 r)





//Start = f3 t2 //(Node (Person "hh_qualify" (2001,11,22)) (Node (Person "hr_qualify" (2001,11,21)) Leaf Leaf) (Node (Person "ht" (2001,11,23)) Leaf Leaf))
//Start = f3 t3  //(Node (Person "hh_qualify" (2001,11,22)) (Node (Person "hr_qualify" (2001,11,21)) (Node (Person "hh" (2002,11,22)) Leaf Leaf) (Node (Person "hh_qualify" (1998,11,22)) Leaf Leaf)) (Node (Person "ht" (2001,11,23)) Leaf Leaf))













