data Nat = Zero | Succ Nat deriving (Eq)

instance Show Nat where
    show nat = show (value nat) ++ " : Nat"
        where value Zero = 0
              value (Succ k) = 1 + value k

plus :: Nat -> Nat -> Nat
plus Zero y = y
plus (Succ k) y = Succ (plus k y)

mult  :: Nat -> Nat -> Nat
mult  Zero _  = Zero
mult (Succ k) y = plus y (mult k y)

one = Succ Zero
two = one `plus` one
three = two `plus` one
four = three `plus` one

nats = iterate Succ Zero

main = do
    print $ two `plus` three
    print $ two `mult` three
