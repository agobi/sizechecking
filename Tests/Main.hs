module Tests.Main where

import qualified Tests.LambdaTest (tests)
import qualified Tests.OpsTest (tests)
import qualified Tests.SizeTest (tests)
import qualified Tests.ExpTest (tests)
import Control.Monad

tests :: [IO Bool]
tests = Tests.LambdaTest.tests 
     ++ Tests.OpsTest.tests
     ++ Tests.SizeTest.tests
     ++ Tests.ExpTest.tests

main :: IO ()
main = liftM and ( sequence tests ) >>= print

