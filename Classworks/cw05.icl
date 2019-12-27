module cw05
import StdEnv

// Calculate Euler's totient function phi(m).
// Euler's so-called totient function phi(m) is defined as 
// the number of positive integers r (1 <= r < m) that are coprime to m.
// Use list compherension
primeif :: Int Int Int-> Bool
primeif m n 1 = True
primeif m n x 
| ( m rem x == 0 ) && (n rem x == 0) = False
= primeif m n (x-1)

euler :: Int -> Int
euler m 
=length [x\\x<-[1..m]|primeif x m x] 

//Start = euler 10 // 4
//Start = euler 100 // 40
//Start = euler 2500 // 1000
//Start = euler 1181 // 1180
// Start = euler 1021904 // 443904


// Generate the list of all possible (Day, Month) tuples in a given year.
// Make sure to take care of different number of days in different months.
    // January - 31 days
    // February - 28 days in a common year and 29 days in leap years
    // March - 31 days
    // April - 30 days
    // May - 31 days
    // June - 30 days
    // July - 31 days
    // August - 31 days
    // September - 30 days
    // October - 31 days
    // November - 30 days
    // December - 31 days
// Make sure to take care of leap years. 
// leap year: 
// if (year is not divisible by 4) then (it is a common year)
// else if (year is not divisible by 100) then (it is a leap year)
// else if (year is not divisible by 400) then (it is a common year)
// else (it is a leap year) 
ifLeapyear :: Int -> Bool 
ifLeapyear year
| (year rem 4) <> 0 = False
| (year rem 100) <> 0 = True
| (year rem 400) <> 0 = False
=True


dayMonth :: Int -> [(Int,Int)]
dayMonth year
| ifLeapyear year =sortBy (\(a,b)(c,d)= a<c) ([(x,y)\\x<-[1,3,5,7,8,10,12],y<-[1..31]] ++ [(2,y)\\y<-[1..28]]++ [(x,y)\\ x<-[4,6,7,9,11],y<-[1..30]])
= sortBy (\(a,b)(c,d)= a<c) ([(x,y)\\ x<-[1,3,5,7,8,10,12],y<-[1..31]]++[(2,y)\\y<-[1..29]]++[(x,y)\\ x<-[4,6,7,9,11],y<-[1..30]])


//Start = dayMonth 2016


// You are given record representing set Q (rational numbers)
// Write function simplifyRational that takes rational number and brings it to normal form. 
// So 15/20 should be 3/4, 2/4 should be 1/2, ...


:: Q = { num :: Int, denom :: Int }

simplifyRational :: Q -> Q
simplifyRational s
| (s.num > 0 && s.denom <0) = {num = 0 - s.num/(gcd s.num (abs s.denom)),denom = abs (s.denom/(gcd s.num (abs s.denom)))}
={num = s.num/(gcd s.num s.denom),denom = s.denom/(gcd s.num  s.denom)}



//Start = simplifyRational { num = 15, denom = 20 } // (Q 3 4)
//Start = simplifyRational { num = 2, denom = 4 } // (Q 1 2)
//Start = simplifyRational { num = 1, denom = 3 } // (Q 1 3)
//Start = simplifyRational { num = 5, denom = 1 } // (Q 5 1)
//Start = simplifyRational { num = 15, denom = -20} // (Q -3 4)