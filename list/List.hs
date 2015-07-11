import Control.Exception (assert)

data List a = Nil | Cons a (List a) deriving Eq

foldLeft :: b -> (b -> a -> b) -> List a -> b
foldLeft b _ Nil = b
foldLeft b f (Cons h t) = foldLeft (f b h) f t

size :: List a -> Int
size = foldLeft 0 (\n -> const (n + 1))

nth :: Int -> List a -> Maybe a
nth n = fst . foldLeft (Nothing, n) f
  where
    f (Nothing, n) value | n <= 0 = (Just value, n - 1)
    f (result, n) _ = (result, n - 1)

reversed :: List a -> List a
reversed = foldLeft Nil (flip Cons)

append :: List a -> List a -> List a
append Nil list = list
append (Cons head tail) list = Cons head (append tail list)

list :: [a] -> List a
list = foldr Cons Nil

main :: IO ()
main = let foo = list [0, 1, 2] in
  assert (size foo == 3) $
  assert (nth 1 foo == Just 1) $
  assert (nth 2 foo == Just 2) $
  assert (nth 3 foo == Nothing) $
  assert (nth 3 foo == Nothing) $
  assert (append foo (list [3, 4, 5]) == list [0, 1, 2, 3, 4, 5]) $
  assert (reversed foo == list [2, 1, 0]) $
  return ()
