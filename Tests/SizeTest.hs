{-# LANGUAGE NoMonomorphismRestriction #-}

module Tests.SizeTest where

import Size
import Lambda
import Ops
import Prelude ( ($), Int, (==), return, sequence, (>>=), and, (.), IO, Bool, String, const )
import qualified Control.Monad

testEmpty1 :: (Size l) => l [Unsized]
testEmpty1 = list (lit 0) (lam $ const unsized)

testNil :: (Size l) => l [a]
testNil = list (lit 0) (lam $ const bottom)

testHead :: (Size l) => l ([a] -> a)
testHead = slam $ \s f -> f `app` (s - lit 1)

testTail :: (Size l) => l ([a] -> [a])
testTail = slam $ \s f -> list (s - lit 1) f

testAddOne :: (Size l) => l ([Unsized] -> [Unsized])
testAddOne = slam $ \s f -> list (s + lit 1) (lam $ const unsized)

testCons :: Size l => l (a -> [a] -> [a])
testCons = lam $ \x -> slam $ \s f ->
    list (s + lit 1) $ shift f s (lam $ const x)

testConcat :: Size l => l ([a] -> [a] -> [a])
testConcat = slam $ \s1 f1 -> slam $ \s2 f2 ->
    list (s1 + s2) $ shift f1 s1 f2


checkAst :: S SData a -> String -> IO Bool
checkAst exp repr = ast exp >>= (\t -> return $ t "" == repr)

tests :: [IO Bool]
tests = [
      checkAst testEmpty1 "List 0 (λa.U)"
    , checkAst testNil "List 0 (λa.┴)"
    , checkAst testHead "Λa,b.b (a-1)"
    , checkAst testTail "Λa,b.List (a-1) b"
    , checkAst testCons "λa.Λb,c.List (b+1) (Shift c b (λd.a))"
    ]

runTests :: IO Bool
runTests = Control.Monad.liftM and $ sequence tests
