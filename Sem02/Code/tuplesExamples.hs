fst :: (String, Int) -> String
fst (a, _) = a

snd :: (String, Int) -> Int
snd (_, b) = b

-- (name, age, course)
type Student = (String, Int, Int)

getAge :: Student -> Int
getAge (_, age, _) = age

compareByAge :: Student -> Student -> Student
compareByAge s1@(_, age1, _) s2@(_, age2, _) = if age1 < age2 then s2 
                                               else s1