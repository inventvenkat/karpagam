{-# LANGUAGE DefaultSignatures, DeriveAnyClass #-}

module Lh where


lucky :: (Integral a) => a -> String
lucky x 
    | y == 7    = "LUCKY NUMBER SEVEN!"
    | y == 123  = "UNLUCKY NUMBER SEVEN!"
    | y == 12   = "you should do something!"
    | otherwise = "Sorry, you're out of luck, pal!"
    where y = x + 1


-- lucky _ = "Sorry, you're out of luck, pal!"

factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)

fir :: (a,a,a) -> a
fir (a,_,_) = a

sec :: (a,a,a) -> a
sec (_,a,_) = a

thi :: (a,a,a) -> a
thi (_,_,a) = a

error = lucky 7

data Shape = Circle Float Float Float deriving (Show)

getShape = Circle 1.2 2.3 1.2

head' :: [Int] -> Int
head' [] = 0
head' (x:_) = x

-- 1:[2,3,4]

initials :: String -> String -> String  
initials (f:_) (l:_) = (f : ". ") ++ [l] ++ "."



-- :t (++)
-- [a] -> [a] -> [a]

-- [1,2,3] ++ [4,5,6] :: [Int]
-- "Hello " ++ "there!" :: [Char]
-- 'H'

applyTwice :: (Int -> Int) -> Int -> Int
applyTwice func x = func x + func x

add5 :: Int -> Maybe Int
add5 0 = Nothing
add5 x = Just $ x + 5

add15 :: Int -> Int
add15 x = x + 15

add16 :: Int -> Int
add16 asldkf = asldkf + 16

doFunc1 = applyTwice (\h -> h + 15) 6
doFunc2 = applyTwice add16 6


data IT a = IT a deriving (Show, Karpagam)
data ECE a = ECE a
data CS a = CS a

instance Functor IT where
    fmap func (IT a) = IT (func a)

class Functor f => Karpagam f where
    graduate :: (a -> b) -> f a -> f b
    graduate func a = func <$> a


add5and6 = (+) 5 6

add6 = (+) 6

-- 5 + 6 === 5 `add` 6 === add 5 6 === (+) 5 6
