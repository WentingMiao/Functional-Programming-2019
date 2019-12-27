module hw06
import StdEnv

:: Person = {name::String, age::Int}

Tringa :: Person
Tringa = {name = "Tringa", age = 42}
Hossam :: Person
Hossam = {name = "Hossam", age = 69}
Nicola :: Person
Nicola = {name = "Nicola", age = 9001}
Zuka :: Person
Zuka = {name = "Zuka", age = 20}

/*
Write a function that finds the older of two people.
*/
older :: Person Person -> String
older p1 p2
| p1.age > p2.age = p1.name
= p2.name

//Start = older Tringa Hossam //"Hossam"
//Start = older Zuka Nicola //"Nicola"