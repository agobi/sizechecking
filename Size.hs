{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Size where

import Lambda
import Ops
import Data.Supply as S

data Unsized

class (LOps l) => Size l where
    list :: l Int -> l (Int -> a) -> l [a]
    slam :: (l Int -> l (Int -> a) -> l b) -> l ([a] -> b)
    shift :: l (Int -> a) -> l Int -> l (Int -> a) -> l (Int -> a)
    unsized :: l Unsized
    bottom :: l a


instance Size S where
    unsized = S $ \_ _ -> showChar 'U'
    bottom = S $ \_ _ -> showChar '┴'
    list size sexp = S $ \s p ->
        let (s1, s2) = S.split2 s
        in showParen (p>0) $
            showString "List " .
            unS size s1 9 .
            showChar ' ' .
            unS sexp s2 9
    slam f = S $ \s p ->
        let (s1, s2, s3) = S.split3 s
            v1           = S.supplyValue s1
            showV1       = S $ \_ _ -> showVar v1
            v2           = S.supplyValue s2
            showV2       = S $ \_ _ -> showVar v2
        in showParen (p>0) $
            showChar 'Λ' .
            showVar v1 .
            showChar ',' .
            showVar v2 .
            showChar '.' .
            unS (f showV1 showV2) s3 0
    shift e1 ss e2 = S $ \s p ->
        let (s1, s2, s3) = S.split3 s
        in showParen (p>0)
        $ showString "Shift "
        . unS e1 s1 2
        . showChar ' '
        . unS ss s2 2
        . showChar ' '
        . unS e2 s3 2

