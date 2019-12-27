module cw07
import StdEnv


:: Gender = Male | Female | Trans | PreferNotToTell
:: Survey = {hateFP :: Bool, gender :: Gender}


notTell :: Gender ->  Bool
notTell PreferNotToTell = True
notTell _ = False

CountAnonymousHater :: [Survey] -> Int
CountAnonymousHater aList  
= length [x \\x<-aList|( x.hateFP == True) && (notTell x.gender)]





//Start = CountAnonymousHater [{hateFP = True, gender = Male}, {hateFP = True, gender = PreferNotToTell}, {hateFP = False, gender = PreferNotToTell}, {hateFP = False, gender = PreferNotToTell}] // 1



// Given a list of distinct name and a list of grade. 2 lists have the same length
// Generate a list of Person corresponding to the name list
// the grades of all Person should be the average of the 2nd list.
// Hint: The record Person should contain at least `name` and `average grade`
:: Person = { name :: String , aveGrade :: Real } 
Generator :: [String] [Int] -> [Person]
Generator nameList gradeList
= [{name = n, aveGrade = (toReal (sum gradeList))/ (toReal (length gradeList)) }\\ n <- nameList ] 


//Start = Generator ["Evan", "Tringa"] [1, 4] // [(Person "Evan" 2.5), (Person "Tringa" 2.5)]
//Start = Generator ["Evan", "Tringa", "Viktoria"] [1, 4, 7] // [(Person "Evan" 4.0), (Person "Tringa" 4.0), (Person "Viktoria", 4.0)]




// Define records Point with real coordinates
// Given 3 Points A B C, decide whether B lies on the segment AC or not.
// Hint: AB + BC = AC
:: Point = { x :: Real , y :: Real  }
OnSegment :: Point Point Point -> Bool
OnSegment p1 p2 p3
| ((p2.x - p1.x)+( p3.x - p2.x) == (p3.x - p1.x) )&& ((p2.y - p1.y) + ( p3.y - p2.y) == (p3.y - p1.y) ) = True
=False

//Start = OnSegment {x = 0.0, y = 0.0} {x = 1.0, y = 1.0} {x = 2.0, y = 2.0}




















