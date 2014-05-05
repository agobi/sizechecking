{-# LANGUAGE FlexibleInstances, KindSignatures #-}

module Ops where

import qualified Data.Supply as S
import Prelude (String, Int, ($), (.))
import qualified Prelude
import Lambda

-- infix operatorok, ezek beagyazasa picit necces, lasd a type family kesobb
class (Lambda l) => LOps l where
    infixop  :: String -> Int -> (a -> b -> c) -> l a -> l b -> l c
    infixopr :: String -> Int -> (a -> b -> c) -> l a -> l b -> l c
    infixopl :: String -> Int -> (a -> b -> c) -> l a -> l b -> l c
    fun ::  String -> a -> l a

(+) :: (LOps l) => l Int -> l Int -> l Int
(+) = infixopl "+" 6 (Prelude.+)

(-) :: (LOps l) => l Int -> l Int -> l Int
(-) = infixopl "-" 6 (Prelude.-)

(*) :: (LOps l) => l Int -> l Int -> l Int
(*) = infixopl "*" 7 (Prelude.*)



instance LOps Q where
    infixopl _ _ f lhs rhs = Q (eval lhs `f` eval rhs)
    infixop  _ _ f lhs rhs = Q (eval lhs `f` eval rhs)
    infixopr _ _ f lhs rhs = Q (eval lhs `f` eval rhs)
    fun _ = Q

instance LOps S where
    fun name _ = S (\_ p -> Prelude.showsPrec p name)

    infixopl name prec _ lhs rhs = S(\s p ->
        let (s1, s2) = S.split2 s
        in Prelude.showParen (p Prelude.> prec) $
            unS lhs s1 prec .
            Prelude.showString name .
            unS rhs s2 (Prelude.succ prec)
        )
    infixop name prec _ lhs rhs = S(\s p ->
        let (s1, s2) = S.split2 s
        in Prelude.showParen (p Prelude.> prec) $
            unS lhs s1 (Prelude.succ prec) .
            Prelude.showString name .
            unS rhs s2 (Prelude.succ prec)
        )
    infixopr name prec _ lhs rhs = S(\s p ->
        let (s1, s2) = S.split2 s
        in Prelude.showParen (p Prelude.> prec) $
            unS lhs s1 (Prelude.succ prec) .
            Prelude.showString name .
            unS rhs s2 prec
        )
