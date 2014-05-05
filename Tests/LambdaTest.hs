{-# LANGUAGE NoMonomorphismRestriction #-}

module Tests.LambdaTest where

import Lambda
import Prelude ( ($), Int, (==), return, sequence, (>>=), and, (.), IO, Bool )
import qualified Control.Monad

test1 :: (Lambda l) => l Int
test1 = app (lam $ \_ -> lit 3) (lit 2)

const2 :: (Lambda l) => l ( Int -> Int )
const2 = lam $ \_ -> lit 2

t3 :: (Lambda l) => l ( (a -> a) -> (a -> a) )
t3 = (lam $ \f -> lam $ \v -> f `app` (f `app` (f `app` v)))


test1ast :: IO Bool
test1ast = do
    t <- ast test1
    return $ t "" == "(λa.3) 2"

test1eval :: IO Bool
test1eval = return $ eval test1 == 3

tests :: [IO Bool]
tests = [ 
      test1ast
    , test1eval
    ]

runTests :: IO Bool
runTests = Control.Monad.liftM and $ sequence tests
