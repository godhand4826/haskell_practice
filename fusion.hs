{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
import Prelude hiding (map, filter, foldl)
import Data.Time

type Reducer b a = b -> a -> b

foldl :: Reducer b a -> b -> [a] -> b
foldl _ b [] = b
foldl r b (a:as) = foldl r (r b a) as

append :: Reducer [a] a
append as a = as ++ [a]

map :: forall a b. (a -> b) -> [a] -> [b]
map f as = foldl r [] as
    where r :: Reducer [b] a
          r bs a = append bs (f a)

filter :: forall a b. (a -> Bool) -> [a] -> [a]
filter p as = foldl r [] as
    where r :: Reducer [a] a
          r as a = if p a then append as a else as

mapping :: (a -> b) -> Reducer c b -> Reducer c a
mapping f r c a = r c (f a)

filtering :: (a -> Bool) -> Reducer b a -> Reducer b a
filtering p r b a = if p a then r b a else b

spendTime :: a -> IO ()
spendTime a = do
    start <- getCurrentTime
    let !x = a
    end <- getCurrentTime
    print $ diffUTCTime end start

main = do
    let !animals = take 15000 $ cycle ["cat","bird","pig","dog","fish"]
    putStrLn "map map"
    spendTime $ map (+10) . map length $ animals
    spendTime $ map ((+10) . length) $ animals
    spendTime $ foldl (mapping length . mapping (+10) $ append) [] animals

    putStrLn "filter filter"
    spendTime $ filter ('g' `elem`) . filter ('i' `elem`) $ animals
    spendTime $ filter (\x -> ('g' `elem` x ) && ('i' `elem` x)) $ animals
    spendTime $ foldl (filtering ('i' `elem`) . filtering ('g' `elem`) $ append) [] animals

    putStrLn "map filter map"
    spendTime $ map (+10) . filter even . map length $ animals
    putStrLn "?"
    spendTime $ foldl (mapping length . filtering even . mapping (+10) $ append) [] animals
