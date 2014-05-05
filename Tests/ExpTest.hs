{-# LANGUAGE NoMonomorphismRestriction #-}

module Tests.ExpTest where

import Lambda
import Exp
import Prelude ( ($), Int, (==), return, sequence, (>>=), and, (.), IO, Bool, String, const )
import qualified Control.Monad

testNil :: Exp e => e [a]
testNil = nil

testAddOne :: Exp e => e ([Int] -> [Int])
testAddOne = lam $ \l -> cons (lit 1) l

testHead :: Exp e => e ([a] -> a)
testHead = lam $ \l -> match l undefined const

testTail :: Exp e => e ([a] -> [a])
testTail = lam $ \l -> match l undefined $ \_ xs -> xs

testConcat :: Exp e => e ([a] -> [a] -> [a])
testConcat = lam $ \l1 -> lam $ \l2 -> match l1 l2
    $ \x xs -> cons x (testConcat `app` xs `app` l2)

testDCons :: Exp e => e [Int]
testDCons = cons (lit 1) $ cons (lit 2) nil

testD2Cons :: Exp e => e [[Int]]
testD2Cons = cons (cons (lit 1) nil) nil

checkAST :: S a -> String -> IO Bool
checkAST exp repr = ast exp >>= (\t -> return $ t "" == repr )

tests :: [ IO Bool ]
tests = 
    [ return $ ([]::[Int]) == eval testNil
    , checkAST testNil "[]"
    , return $ [1::Int] == eval (testAddOne `app` nil)
    , checkAST testAddOne "λa.1:a"
    , return $ [2..6::Int] == eval testTail [1..6]
    , return $ [1..6::Int] == eval testConcat [1,2,3] [4,5,6]
    , return $ [1,2::Int] == eval testDCons
    , checkAST testDCons "1:2:[]"
    , return $ [[1::Int]] == eval testD2Cons
    , checkAST testD2Cons "(1:[]):[]"
    ]


runTests :: IO Bool
runTests = Control.Monad.liftM and $ sequence tests
