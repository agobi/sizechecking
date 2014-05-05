module Exp where

import Lambda
import Ops
import Data.Supply as S

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
instance Exp S where
    nil = S $ \_ _ -> showString "[]"
    undefined = S $ \_ _ -> showString "undefined"
    match list nbranch cbranch = S $ \s p ->
        let (s1, s2, ss) = S.split3 s
            (s3, s4, s5) = S.split3 ss
            v1           = S.supplyValue s4
            showV1       = S $ \_ _ -> showVar v1
            v2           = S.supplyValue s5
            showV2       = S $ \_ _ -> showVar v2
        in showParen (p>0) $ 
            showString "case ".
            unS list s1 0 .
            showString " of [] => ".
            unS nbranch s2 0 .
            showString "; (" .  showVar v1 . showChar ':' . showVar v2 . showString ") => " .
            unS (cbranch showV1 showV2) s3 0
    cond c tbranch fbranch = S $ \s p ->
        let (s1, s2, s3) = S.split3 s
        in showParen (p>0) $ 
            showString "if ".
            unS c s1 0 .
            showString " then " .
            unS tbranch s2 0 .
            showString " else " .
            unS fbranch s3 0
