-- @author kukimukiku
-- Functional programming exercise set1

module Main where 

import Test.QuickCheck

import System.IO
import Test.QuickCheck


-- EXERCISE 1

-- nAnd function version 1 
nAnd :: Bool -> Bool -> Bool
nAnd x y = not ( x && y )

-- nAnd function version 2
nAnd2 :: Bool -> Bool -> Bool
nAnd2 True x = not x
--nAnd2 x True = not x
nAnd2 False _ = True

-- nAnd function version 3 (truth table)
nAnd3 True True = False
nAnd3 _ _ = True
--nAnd3 False True = True
--nAnd3 False False = True



-- EXERCISE 2

-- additional property
nAnd4 :: Bool -> Bool -> Bool
nAnd4 x y = not x || not y 

-- checking same functions
prop_nAnd :: Bool -> Bool -> Bool
prop_nAnd x y =
    nAnd x y == (nAnd2 x y && nAnd3 x y)

-- checking with additional function
prop_nAnd2 :: Bool -> Bool -> Bool
prop_nAnd2 x y =
    nAnd x y == (nAnd2 x y && nAnd4 x y)



-- EXERCISE 3

nDigits :: Integer -> Int
nDigits x
    | x >= 0 = length(show x)
    | x < 0 = nDigits(abs x)



-- EXERCISE 4

nRoots :: Float -> Float -> Float -> Int
nRoots a b c
    | a == 0 = error "First argument can't be zero"
    | (b^2) > 4.0 * a * c = 2
    | (b^2) == 4.0 * a * c = 1
    | (b^2) < 4.0 * a * c = 0



-- EXERCISE 5

getRoots :: Float -> Float -> Float -> (Float, Float)
getRoots a b c 
    | (nRoots a b c) == 2 && a > 0 = ((smallerRoot a b c), (largerRoot a b c))
    | (nRoots a b c) == 2 && a < 0 = ((largerRoot a b c), (smallerRoot a b c))
    | (nRoots a b c) == 1 = (smallerRoot a b c, smallerRoot a b c)
    | (nRoots a b c) == 0 = (smallerRoot a b c, smallerRoot a b c)

smallerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c = (-b - sqrt(b^2 - 4 * a * c))/2 * a

largerRoot :: Float -> Float -> Float -> Float
largerRoot a b c = (-b + sqrt(b^2 - 4 * a * c))/2 * a



-- EXERCISE 6

power2 :: Integer -> Integer
power2 x
    | x == 0 = 1
    | x > 0 = 2 * power2(x-1)
    | x < 0 = 0



-- EXERCISE 7

mult :: Integer -> Integer -> Integer
mult m n
    | m == 0 = 0
    | n == 0 = 0
    | m > 0 && n > 0 = m + mult m (n - 1)
    | n < 0 && m < 0 = a + mult a (b - 1)
    | n < 0 || m < 0 = -1 * mult a b
    where a = abs m
          b = abs n


-- 1 - 4   ->>> 1 * 2 * 3 * 4

-- EXERCISE 8

prod :: Integer -> Integer -> Integer
prod m n
    | n > m = m * prod (m + 1) n
    | m > n = error "Bad range"
    | m == n = n

fac :: Integer -> Integer
fac n = prod 1 n

main = do 
    putStrLn "First implementation results..."
    print (nAnd True True)
    print (nAnd False True)
    print (nAnd True False)
    print (nAnd False False)

    putStrLn ""
    putStrLn "Second implementation results..."
    print (nAnd2 True True)
    print (nAnd2 False True)
    print (nAnd2 True False)
    print (nAnd2 False False)

    putStrLn ""
    putStrLn "Truth table results"
    print (nAnd3 True True)
    print (nAnd3 False True)
    print (nAnd3 True False)
    print (nAnd3 False False)

    putStrLn ""
    putStrLn "QuickCheck that should pass..."
    quickCheck prop_nAnd

    putStrLn ""
    putStrLn "QuickCheck that should fail..."
    quickCheck prop_nAnd2

    putStrLn ""
    putStrLn "172738288 digit count..."
    print (nDigits 172738288)

    putStrLn ""
    putStrLn "-172738288 digit count..."
    print (nDigits (-172738288))

    putStrLn ""
    putStrLn "When a = 1, b = 4, c = 1, quadratic equation has this many roots..."
    print (nRoots 1 4 1)

    putStrLn ""
    putStrLn "When a = 4, b = 4, c = 1, quadratic equation has this many roots..."
    print (nRoots 4 4 1)

    putStrLn ""
    putStrLn "When a = 4, b = 4, c = 4, quadratic equation has this many roots..."
    print (nRoots 4 4 4)

    putStrLn ""
    putStrLn "When a = 1, b = 4, c = 1, these are the roots..."
    print (getRoots 1 4 1)

    putStrLn ""
    putStrLn "When a = -1, b = 4, c = 1, these are the roots..."
    print (getRoots (-1) 4 1)

    putStrLn ""
    putStrLn "When a = 4, b = 4, c = 1, these are the roots..."
    print (getRoots 4 4 1)

    putStrLn ""
    putStrLn "When a = 1, b = 4, c = 1, these are the roots..."
    print (getRoots 4 4 4)

    putStrLn ""
    putStrLn "2^4"
    print (power2 4)

    putStrLn ""
    putStrLn "2^-4"
    print (power2 (-4))

    putStrLn ""
    putStrLn "3 * 4 = ..."
    print (mult 3 4)

    putStrLn ""
    putStrLn "3 * -4 = ..."
    print (mult (-4) 3)

    putStrLn ""
    putStrLn "-3 * 4 = ..."
    print (mult 4 (-3))

    putStrLn ""
    putStrLn "-3 * -4 = ..."
    print (mult (-4) (-3))

    putStrLn ""
    putStrLn "Range [1, 4] multiplied is  ..."
    print (prod 2 4)

    putStrLn ""
    putStrLn "Special case (?), range [1, 5], using special factorial case  ..."
    print (prod 1 5)

    putStrLn ""
    putStrLn "5 factorial is  ..."
    print (fac 5)

    putStrLn ""

