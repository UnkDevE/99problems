myLast :: [a] -> a
myLast [] = error "can't find last of empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "can't find penultimate of empty list"
myButLast [x] = error "list too short"
myButLast (x:xs) 
    | length(xs) == 1 = x
    | otherwise = myButLast xs

elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)

myLength :: [a] -> Int 
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse l = 
    [l !! (length(l)-num) | num <- [1..length(l)]]

isPalindrome l =
    l == reverse l
