-- Tarefa 01
-- elem :: (Eq a, Foldable t) => a -> t a -> Bool
-- sum :: (Num a, Foldable t) => t a -> a
-- minimum :: (Ord a, Foldable t) => t a -> a

--Tarefa 02

data Part = AM | PM
    deriving (Eq, Show)

data Time = Local Int Int Part
          | Total Int Int

hora1 = Local 10 20 PM
hora2 = Local 6 48 AM
hora3 = Total 15 30

totalMinutos (Total x y) = x * 60 + y
totalMinutos (Local x y PM) = (12 + x) * 60 + y
totalMinutos (Local x y _) = x * 60 + y

instance Eq Time where
    x == y = (totalMinutos x) == (totalMinutos y)

instance Show Time where
    show (Local x y AM) = show x ++ ":" ++ show y ++ " am"
    show (Local x y PM) = show x ++ ":" ++ show y ++ " pm"
    show (Total x y) = show x ++ "h" ++ show y ++ "m"

instance Ord Time where
    x > y = (totalMinutos x) > (totalMinutos y)
    x >= y = (totalMinutos x) >= (totalMinutos y)
    x < y = (totalMinutos x) < (totalMinutos y)
    x <= y = (totalMinutos x) <= (totalMinutos y)
    min x y = if x > y then y else x
    max x y = if x > y then x else y

seleciona _ [] = []
seleciona time ((movieTime, movie):xs)
    | movieTime > time = ((movieTime, movie):(seleciona time xs))
    | otherwise = seleciona time xs

instance Enum Time where