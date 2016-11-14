{-# LANGUAGE UnicodeSyntax #-}

module Combinators where

import           Prelude.Unicode

import           Control.Applicative (liftA2, (<*>))
import           Control.Arrow
import           Data.Function       (on)

-- p = paraller, c = compose, m = f x x

-- c12 f g = \x y → f (g x y)
c12 ∷  (Functor f, Functor f1) ⇒ (a → b) → f (f1 a) → f (f1 b)
c12 = fmap ∘ fmap

-- c21 f g = \x y → f x (g y)
c21 ∷ (Applicative f, Functor f1) ⇒ f (a → b) → f (f1 a) → f (f1 b)
c21 = liftA2 fmap

-- c21m f g = \x → (hook f g) x x
c21m ∷ Applicative f ⇒ f (a → b) → f a → f b
c21m = (<*>)

-- c2p1 f g h = \x → f (g x) (h x)
c2p1 ∷ Applicative f ⇒ (a → b → c) → f a → f b → f c
c2p1 = liftA2

-- c2p1m f g = \x y → f (g x) (g y)
c2p1m ∷ (b → b → c) → (a → b) → a → a → c
c2p1m = on

-- c2p2 f g h = \x y → f (g x y) (h x y)
c2p2 ∷ (Applicative f, Applicative f1) ⇒
     (a → b → c) → f1 (f a) → f1 (f b) → f1 (f c)
c2p2 = liftA2 ∘ liftA2

-- p1 f g = \x → ((f x), (g x))
p1 ∷ Arrow a => a b c -> a b c' -> a b (c, c')
p1 = (&&&)

-- p2 f g = \x y → ((f x y), (g x y))
p2 ∷ (Arrow a, Applicative f) ⇒ f (a b c) → f (a b c') → f (a b (c, c'))
p2 = liftA2 (&&&)

addTuple ∷ (Arrow a) ⇒ a (ℤ, ℤ) ℤ
addTuple = arr (uncurry (+))

addArgs ∷ (Arrow a1, Arrow a2) ⇒ a1 ℤ (a2 ℤ ℤ)
addArgs = arr (arr ∘ (+))

arr2t ∷ (Arrow a0) ⇒ (a → b → c) → a0 (a, b) c
arr2t = arr ∘ uncurry

arr2a ∷ (Arrow a0, Arrow a1) ⇒ (a → b → c) → a0 a (a1 b c)
arr2a = arr ∘ (arr ∘)
