module cw06
import StdEnv

// A Pythagorean triad is a triple of integers (a,b,c) such that
// a^2 + b^2 == c^2
// Count how many triads are there with 1<=a<=b<=c<=n.
// n is given as parameter
// Hint/Requirement: Use list comprehension
// Hint: To avoid three loops, check if a*a + b*b is perfect square, if yes increase count. 
// Also, you have to check that sqrt(a * a + b * b) <= n. No need to check sqrt(a * a + b * b)>=a,b, it is automatic. Think why.
isPerfectSquare :: Int Int -> Bool
isPerfectSquare n x
| x > toInt (sqrt (toReal n)) = False
| x*x == n = True
= isPerfectSquare n (x+1)


countAll :: Int -> Int
countAll max 
=length[(a,b,toInt(sqrt (toReal (a*a+b*b))))\\a<-[1..max],b<-[1..max]|isPerfectSquare (a*a+b*b) 1 && toInt(sqrt (toReal (a*a+b*b)))<= max && (a<=b)]



Start = countAll 100 // 52
//Start = countAll 1442 // 1349
//Start = countAll 3134 // 3334

// You are given a point and triangle. Determine if point lies inside triangle.
// Hint: https://stackoverflow.com/questions/2049582/how-to-determine-if-a-point-is-in-a-2d-triangle
// You have to write records for Point and Triangle. Point should be represented as two Real coordinates x and y. 
// Triangle is represented as three Points a,b and c.
:: Point = { x :: Real, y :: Real}
:: Triangle = {a :: Point ,b :: Point, c :: Point}

sign :: Point Point Point -> Real
sign p1 p2 p3
= (p1.x - p3.x) * (p2.y - p3.y) - (p2.x - p3.x) * (p1.y - p3.y)


isInside :: Point Triangle -> Bool
isInside pt tri
| ((sign pt tri.a tri.b < 0.0)||(sign pt tri.b tri.c < 0.0)||(sign pt tri.c tri.a < 0.0))&&((sign pt tri.a tri.b > 0.0)||(sign pt tri.b tri.c > 0.0)||(sign pt tri.c tri.a > 0.0))= False
=True

isInside2 :: Point Triangle ->Bool
isInside2 pt tri
| ((d1 < 0.0) || (d2 < 0.0) || (d3 < 0.0) ) && ((d1 > 0.0) || (d2 > 0.0) || (d3 > 0.0)) = False
=True
where
	d1 = (sign pt tri.a tri.b)
	d2 = (sign pt tri.b tri.c)
	d3 =(sign pt tri.c tri.a)

//Start = sign {x = 1.0, y = 1.0} {x = -1.0, y = 1.0} {x = 0.0, y = -1.0}







//Start = isInside2 {x = 0.0, y = 0.0} {a = {x = 1.0, y = 1.0}, b = {x = -1.0, y = 1.0}, c = {x = 0.0, y = -1.0}} // True
//Start = isInside2 {x = 3.0, y = 4.0} {a = {x = 1.0, y = 1.0}, b = {x = -1.0, y = 1.0}, c = {x = 0.0, y = -1.0}} // False
//Start = isInside2 {x = 0.0, y = 0.0} {a = {x = 0.0, y = 0.0}, b = {x = 0.0, y = 0.0}, c = {x = 0.0, y = 0.0}} // True
// Start = isInside2 {x = 0.0, y = 1.0} {a = {x = -1.0, y = 1.0}, b = {x = 1.0, y = 1.0}, c = {x = 1.0, y = 1.0}} // True


/*bool PointInTriangle (fPoint pt, fPoint v1, fPoint v2, fPoint v3)
{
    float d1, d2, d3;
    bool has_neg, has_pos;

    d1 = sign(pt, v1, v2);
    d2 = sign(pt, v2, v3);
    d3 = sign(pt, v3, v1);

    has_neg = (d1 < 0) || (d2 < 0) || (d3 < 0);
    has_pos = (d1 > 0) || (d2 > 0) || (d3 > 0);

    return !(has_neg && has_pos);
}

*/





