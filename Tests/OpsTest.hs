{-# LANGUAGE NoMonomorphismRestriction #-}

module Tests.OpsTest where

import Ops
import Lambda
import Prelude ( ($), Int, (==), return, sequence, (>>=), and, (.), IO, Bool )
import qualified Control.Monad

test1 :: (LOps l) => l Int
test1 = app (lam $ \x -> lit 3 + x) (lit 2)

test2 :: (LOps l) => l Int
test2 = app (lam $ \x -> lit 3 * x) (lit 2)

test3 :: (LOps l) => l Int
test3 = app (lam $ \x -> lit 2 * x + lit 1) (lit 5 - lit 2)


test1ast :: IO Bool
test1ast = do
    t <- ast test1
    return $ t "" == "(λa.3+a) 2"

test1eval :: IO Bool
test1eval = return $ eval test1 == 5

test2ast :: IO Bool
test2ast = do
    t <- ast test2
    return $ t "" == "(λa.3*a) 2"

test2eval :: IO Bool
test2eval = return $ eval test2 == 6

test3ast :: IO Bool
test3ast = do
    t <- ast test3
    return $ t "" == "(λa.2*a+1) (5-2)"

test3eval :: IO Bool
test3eval = return $ eval test3 == 7

tests :: [IO Bool]
tests = [ 
      test1ast
    , test1eval
    ]

runTests :: IO Bool
runTests = Control.Monad.liftM and $ sequence tests
