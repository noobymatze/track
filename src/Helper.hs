module Helper
  ((|>)
  ) where


(|>) :: a -> (a -> b) -> b
(|>) a f =
  f a

infixl 5 |>
