{-# LANGUAGE FlexibleInstances, KindSignatures #-}

module Ops where

import qualified Data.Supply as S
import Prelude (String, Int, ($), (.))
import qualified Prelude
import Lambda
import Data.Lens.Light

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

instance (SContext s) => LOps (S s) where
    fun name _ = S (\ctx -> Prelude.showsPrec (getL prec ctx) name)

    infixopl name p _ lhs rhs = S(\ctx ->
        let (s1, s2) = S.split2 (getL supply ctx)
        in Prelude.showParen ((getL prec ctx) Prelude.> p) $
            unS lhs (setL supply s1 $ setL prec p ctx) .
            Prelude.showString name .
            unS rhs (setL supply s2 $ setL prec (Prelude.succ p) ctx)
        )

    infixop  name p _ lhs rhs = S(\ctx ->
        let (s1, s2) = S.split2 (getL supply ctx)
        in Prelude.showParen ((getL prec ctx) Prelude.> p) $
            unS lhs (setL supply s1 $ setL prec (Prelude.succ p) ctx) .
            Prelude.showString name .
            unS rhs (setL supply s2 $ setL prec (Prelude.succ p) ctx)
        )

    infixopr name p _ lhs rhs = S(\ctx ->
        let (s1, s2) = S.split2 (getL supply ctx)
        in Prelude.showParen ((getL prec ctx) Prelude.> p) $
            unS lhs (setL supply s1 $ setL prec (Prelude.succ p) ctx) .
            Prelude.showString name .
            unS rhs (setL supply s2 $ setL prec p ctx)
        )
