{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Size where

import Lambda
import Ops
import Data.Supply as S
import Data.Lens.Light

data Unsized

class (LOps l) => Size l where
    list :: l Int -> l (Int -> a) -> l [a]
    slam :: (l Int -> l (Int -> a) -> l b) -> l ([a] -> b)
    shift :: l (Int -> a) -> l Int -> l (Int -> a) -> l (Int -> a)
    unsized :: l Unsized
    bottom :: l a


instance SContext s => Size (S s) where
    unsized = S $ \_ -> showChar 'U'
    bottom = S $ \_ -> showChar '┴'
    list size sexp = S $ \ctx ->
        let (s1, s2) = S.split2 (getL supply ctx)
            p = getL prec ctx
        in showParen (p>0) $
            showString "List " .
            unS size (updateCtx s1 9 ctx) .
            showChar ' ' .
            unS sexp (updateCtx s2 9 ctx)
    slam f = S $ \ctx ->
        let (s1, s2, s3) = S.split3 (getL supply ctx)
            v1           = S.supplyValue s1
            showV1       = S $ \_ -> showVar v1
            v2           = S.supplyValue s2
            showV2       = S $ \_ -> showVar v2
            p = getL prec ctx
        in showParen (p>0) $
            showChar 'Λ' .
            showVar v1 .
            showChar ',' .
            showVar v2 .
            showChar '.' .
            unS (f showV1 showV2) (updateCtx s3 0 ctx)
    shift e1 ss e2 = S $ \ctx ->
        let (s1, s2, s3) = S.split3 (getL supply ctx)
            p = getL prec ctx
        in showParen (p>0)
        $ showString "Shift "
        . unS e1 (updateCtx s1 2 ctx)
        . showChar ' '
        . unS ss (updateCtx s2 2 ctx)
        . showChar ' '
        . unS e2 (updateCtx s3 2 ctx)

