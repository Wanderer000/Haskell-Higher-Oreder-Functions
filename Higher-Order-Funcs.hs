-- Anton Nikitin

--Higher-Order Functions

-- Applies all functions in the list to a integer

applyAll :: [a -> b] -> a -> [b]
applyAll [] _ = []
applyAll (x:xs) y = (x y) : (applyAll xs y)

-- Creating Property type

type Property a = [a -> Bool]

-- Checking whether integer match with the list of the properties

satisfies :: Property a -> a -> Bool
satisfies f x = and (applyAll f x)

power :: Int -> (a -> a) -> a -> a
power n f x 
           | n == 0 = x
           | otherwise = f (power (n-1) f x)

-- Sum function from scratch

plus :: Int -> Int -> Int
plus f x
        | f == 0 = x
        | otherwise = (power (f) succ x)

-- Multiply function from scratch

times :: Int -> Int -> Int
times f 1 = f
times f x = f + times f (x-1)
