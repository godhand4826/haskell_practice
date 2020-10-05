data Tree a = Nil
            | Node (Tree a) a (Tree a)

-- naive recursive
depth Nil = 0
depth (Node l _ r) = 1 + max (depth l) (depth r)

-- passing state recursively
depthS = f 0
    where f d Nil = d
          f d (Node l _ r) = max (f (d+1) l) (f (d+1) r)

-- continuation passing style
depthCPS t = f t id
    where f Nil k = k 0
          f (Node l _ r) k = f l $ \dl ->
                                f r $ \dr ->
                                    k (1 + max dl dr)

-- defunctionalization (eliminates higher-order functions)
data Fun a = FunL (Tree a) (Fun a) -- r k
           | FunR Int (Fun a) -- dl k
           | FunId

depth' t = f t FunId
    where f :: Tree a -> Fun a -> Int
          f Nil k = eval k 0
          f (Node l _ r) k = f l (FunL r k)

          eval :: Fun a -> Int -> Int
          eval (FunL r k) dl = f r (FunR dl k)
          eval (FunR dl k) dr = eval k (1 + max dl dr)
          eval FunId d = d

main = do
    let root = Node (Node Nil 2 Nil) 1 (Node Nil 3 (Node (Node Nil 5 Nil) 4 Nil))
    print $ depth root
    print $ depthS root
    print $ depthCPS root
    print $ depth' root
