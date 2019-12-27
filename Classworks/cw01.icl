module first
import StdEnv


// Determine the greatest common divisor of two

// positive integer numbers.

// f1 :: Int Int -> Int

// Start = f1 5 15 // 5

// Start = f1 6 11 // 1

// Start = f1 6 15 // 3

fl :: Int Int -> Int
fl a b 
| a < b = f1 b a
= flaux a b b

flaux a b c
| (a rem c == 0) && (b rem c == 0) = c//mod in clean is rem
=flaux a b (c-1)




// You are climbing a stair case. It takes n steps to reach to the top.

// Each time you can either climb 1 or 2 steps.

// In how many distinct ways can you climb to the top?

// f2 :: Int -> Int

// Start = f2 2 // 2 (1+1,2)

// Start = f2 3 // 3 (1+1+1,1+2,2+1)
fac :: Int -> Int
fac i
| i==0 = 1
| i>0 =i* fac (i-1)

f2 :: Int -> Int
f2 n = fac n / ( fac 2 * fac (n-2))





// Given an integer, write a function that returns

// the largest digit in the integer.

// f3 :: Int -> Int

// Start = f3 564 //6

// Start = f3 5 //5


f3 :: Int -> Int
f3aux :: Int Int -> Int

f3 a 
| a<0 = f3(abs a)
| a<10 =a
= f3aux (a/10) (a rem 10)

f3aux a maxDigit
| a<=0 =maxDigit
| (a rem 10)> maxDigit =f3aux (a/10) (a rem 10)
| (a rem 10)< maxDigit =f3aux (a/10) maxDigit

f3 1253

















