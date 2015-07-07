import Control.Exception (assert)

data List a = Nil | Cons a (List a)

size :: List a -> Int
size Nil = 0
size (Cons _ tail) = 1 + size tail

nth :: Int -> List a -> Maybe a
nth _ Nil = Nothing
nth n (Cons head tail) =
  if n <= 0 then
    Just head
  else
    nth (n - 1) tail

main :: IO ()
main = let foo = Cons 0 $ Cons 1 $ Cons 2 Nil in
  assert (size foo == 3) $
  assert (nth 1 foo == Just 1) $
  assert (nth 2 foo == Just 2) $
  assert (nth 3 foo == Nothing) $
  return ()
