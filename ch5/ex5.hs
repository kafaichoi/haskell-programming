-- Type-Kwon-Do

-- Q1

f :: Int -> String
f = undefined
g :: String -> Char
g = undefined
h :: Int -> Char;
h x = g (f x)

-- Q2

data A
data B
data C

q :: A -> B;
q = undefined
w :: B -> C;
w = undefined
e :: A -> C;
e x = w (q x)

-- Q3

data X
data Y
data Z

xz :: X -> Z;
xz = undefined
yz :: Y -> Z;
yz = undefined
xform :: (X, Y) -> (Z, Z);
xform (x, y) = (xz x, yz y)

-- Q4

munge :: (x -> y) -> (y -> (w, z)) -> x -> w; 
munge xy ywz x = fst $ ywz $ xy x
