module Lab2 where

import Data.List (List(..), (:), reverse)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(..),fst,snd)
import Prelude
import Effect (Effect)
import Effect.Console (log)

findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex _ Nil = Nothing
findIndex f (x : xs) = if f x then Just 0 else map ((+) 1) (findIndex f xs)

findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex _ Nil = Nothing
findLastIndex p (x : xs) = case findLastIndex p xs of
  Nothing -> if p x then Just 0 else Nothing
  Just i -> Just (i + 1)

zip :: forall a b. List a -> List b -> List (Tuple a b)
zip Nil _ = Nil
zip _ Nil = Nil
zip (Cons x xs) (Cons y ys) = Cons (Tuple x y) (zip xs ys)

unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip Nil = Tuple Nil Nil
unzip (Tuple a b : xs) = Tuple (a : as) (b : bs) 
  where
    Tuple as bs = unzip xs

filter :: forall a. (a -> Boolean) -> List a -> List a
filter _ Nil = Nil
filter p (x : xs) = if p x then x : filter p xs else filter p xs

tailRecursionFilter :: forall a. (a -> Boolean) -> List a -> List a -> List a
tailRecursionFilter _ Nil resultList = resultList
tailRecursionFilter f (x : xs) resultList = if f x 
  then tailRecursionFilter f xs (x : resultList) 
    else tailRecursionFilter f xs resultList

take :: forall a. Int -> List a -> List a
take n (x : xs) 
    | n == 0 = Nil
    | otherwise = Cons x (take (n - 1) xs)
take _ _ = Nil

tailRecursionTake :: forall a. Int -> List a -> List a -> List a
tailRecursionTake n (x : xs) resultList
    | n == 0 = reverse resultList
    | otherwise = tailRecursionTake (n - 1) xs (x : resultList)
tailRecursionTake _ _ resultList = reverse resultList

test :: Effect Unit
test = do
    log ("findIndex: " <> show (findIndex ((==) 0) (1 : 2 : 0 : 3 : Nil)))
    log ("findIndex: " <> show (findIndex ((==) 0) (1 : 2 : 3 : Nil)))
    log ("findIndex: " <> show (findIndex ((==) 0) Nil))

    log ("findLastIndex: " <> show (findLastIndex (_ == 0) (1 : 2 : 3 : 0 : 4 : Nil)))
    log ("findLastIndex: " <> show (findLastIndex (_ == 0) (1 : 2 : 3 : 4 : Nil)))

    log ("zip: " <> show (zip (1 : 2 : 3 : Nil) (4 : 5 : 6 : Nil)))

    log ("unzip: " <> show (unzip (Tuple 1 2 : Tuple 3 4 : Nil)))

    log ("filter: " <> show (filter (\x -> x > 2) (1 : 2 : 3 : 4 : Nil)))
    log ("filter: " <> show (filter (\x -> x > 2) (1 : 2 : Nil)))

    log ("tailRecursionFilter: " <> show (
      tailRecursionFilter (\x -> x `mod` 2 == 0) (1 : 2 : 3 : 4 : Nil) Nil))
    log ("tailRecursionFilter: " <> show (
      tailRecursionFilter (\x -> x `mod` 2 == 0) (1 : 2 : 3 : 4 : Nil) Nil == (2 : 4 : Nil)))

    log ("take: " <> show (take 3 (1 : 2 : 3 : 4 : 5 : Nil)))
    log ("take: " <> show (take 3 (1 : 2 : Nil)))

    log ("tailRecursionTake: " <> show (take 3 (1 : 2 : 3 : 4 : 5 : Nil)))
    log ("tailRecursionTake: " <> show (take 3 (1 : 2 : Nil)))