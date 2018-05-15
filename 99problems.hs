myLast :: [a] -> a
myLast [] = error "can't find last of empty list"
myLast [x] = x
myLast (x:xs) = 
    myLast xs

myButLast :: [a] -> a
myButLast [] = error "can't find penultimate of empty list"
myButLast [x] = error "list too short"
myButLast (x:xs) 
    | length(xs) == 1 = x
    | otherwise = myButLast xs
