{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module ConCat.Prop where


import qualified GHC.Generics

import ConCat.Rebox ()
import ConCat.AltCat ()
import ConCat.Category

-- Products and Permutations category


-- First we need a symmetric monoidal category.
-- Here's a construction
-- a type parameterizing the collection of objects C
-- a homset of morphisms or a function generating such a homset for all possible morphisms from (x, y) f :: (x -> y).
-- The category has an id and a (.) composition described. Signatures:
       -- id :: a `k` a  (k is the construction morphism)
       -- (.) :: (x ->y) -> (y -> z) -> (x -> z)
              -- should be associative: (f g) h = f (g h)
              -- should obey the right and left unit laws for all f (id f == f == f id)


-- An isomorphism is a morphism f : x -> y and its inverse g : y -> x, such that fg = idx and gf = idy

-- We need a functor to move between categories.
-- Here is the construction for F.

class FunctorC c d where
  fObj :: c -> d
  fMoprhism :: (c -> c) -> (d -> d)
  -- preserves identity :-> forall inC fObj (id . F) = F . id -- do not understand this
  -- preserves composition :-> for any pair of morphisms f : x -> y, g : y -> z in C, F(fg) = F(f)F(g)
  -- TODO: Define an identity functor and composition of functors and check all the laws stated thus far.
  -- Natural Transformation happens between two functors F and G both of type C -> D
            -- a :: F -> G
            -- for f :: x -> y
            -- a F x = G x
            -- F y = (F f) (F x)
            -- a F y = G y
            -- (G f) (G x) = G y

-- A monoidal category consists of
  -- a Category M
  -- a functor defining its tensor product (<>) :: M -> M -> M
  -- QUESTION : How is the tensor product different from the (.) Composition in the type signature? I think is same. But no.
    -- Where M is both objects and morphisms, i.e:
            -- (<>) x y = x <> y
            -- (<>) f g = f <> g
  -- an id object
  -- natural isomorphisms called the associator a :: (x <> y) <> z = x <> (y <> z)
    -- The associator is governed by the pentagon equation (pentagram dajjal lol).
            -- a(w (x <> y) z . (w <> (x <> y)) <> z) = w <> ((x <> y) <> z)
            
  -- left unit law lx :: id <> x == x
  -- right unit law rx :: x <> id == x



-- Instances for Category, MonoidPCat, Braided
  
  




-- A prop is a strict symmetric monoidal category where every object is a product of its id function:

-- x ^ n = x  x . x . x . x ... . x  


-- where for a since object 
