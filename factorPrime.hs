
--Thara Radha Palaniswamy
import Debug.Trace

divisor :: Integer -> [Integer]
divisor num = divisornto1 num num []

divisornto1 :: Integer -> Integer -> [Integer] -> [Integer]
divisornto1 num divnum list=
    if divnum == 1
        then list ++ [1]
    else if num `mod` divnum == 0
        then divisornto1 num (divnum-1) (list ++ [divnum])
    else divisornto1 num (divnum-1) list




primeNumber :: Integer -> Bool
primeNumber n =
    if length (divisor n)== 2 then trace (show n++" is a prime number.\nDivisors are:"++show (divisor n))True else trace (show n++" is not a prime Number.\nDivisors are: "++show (divisor n)) False


