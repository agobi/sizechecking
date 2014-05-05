module Exp where

import Lambda
import Ops
import Data.Supply as S
import Data.Lens.Light

class LOps l => Exp l where
    nil :: l [a]
    cons :: l a -> l [a] -> l [a]
    cons = infixopr ":" 5 (:)
    match :: l [a] -> l b -> (l a -> l [a] -> l b) -> l b
    cond :: l Bool -> l a -> l a -> l a
    undefined :: l a

instance Exp Q where
    nil = Q []
    cons x xs = Q ( unQ x : unQ xs )
    match l nbranch cbranch = case unQ l of
        [] -> nbranch
        (x:xs) -> cbranch (Q x) (Q xs)
    cond c tbranch fbranch = if unQ c then tbranch else fbranch
    undefined = Prelude.undefined 

instance SContext s => Exp (S s) where
    nil = S $ \_ -> showString "[]"
    undefined = S $ \_ -> showString "undefined"
    match list nbranch cbranch = S $ \ctx ->
        let (s1, s2, ss) = S.split3 (getL supply ctx)
            (s3, s4, s5) = S.split3 ss
            v1           = S.supplyValue s4
            showV1       = S $ \_ -> showVar v1
            v2           = S.supplyValue s5
            showV2       = S $ \_ -> showVar v2
            p = getL prec ctx
        in showParen (p>0) $ 
            showString "case ".
            unS list (updateCtx s1 0 ctx) .
            showString " of [] => ".
            unS nbranch (updateCtx s2 0 ctx) .
            showString "; (" .  showVar v1 . showChar ':' . showVar v2 . showString ") => " .
            unS (cbranch showV1 showV2) (updateCtx s3 0 ctx)
    cond c tbranch fbranch = S $ \ctx ->
        let (s1, s2, s3) = S.split3 (getL supply ctx)
            p = getL prec ctx
        in showParen (p>0) $ 
            showString "if ".
            unS c (updateCtx s1 0 ctx).
            showString " then " .
            unS tbranch (updateCtx s2 0 ctx).
            showString " else " .
            unS fbranch (updateCtx s3 0 ctx)
