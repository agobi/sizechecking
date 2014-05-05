{-# LANGUAGE TypeFamilies,MultiParamTypeClasses,FunctionalDependencies,UndecidableInstances,FlexibleInstances,OverlappingInstances,FlexibleContexts,IncoherentInstances #-}

module SizedFun where

import Lambda
import Size
import Exp
import Ops

import qualified Data.Supply as S

class Infer a b where
instance (Infer a b, Infer p q) => Infer (a->p) (b->q)
instance Infer a b => Infer [a] [b]
instance a~b => Infer a b
instance Infer Unsized Int

class (Exp e, Size (SizeExp e)) => SizedFun e where
  type SizeExp e :: * -> *
  bind :: Infer a b => String -> SizeExp e a -> e b -> e b

instance SizedFun S where
  type SizeExp S = S
  bind name size exp = S $ \s _ fs -> if fs then
      showString name
      else let (s1, s2) = S.split2 s
      in showString name . showString " :: " . unS size s1 0 . showChar '\n'.
         showString name . showString " = "  . unSS exp s2 0 False
