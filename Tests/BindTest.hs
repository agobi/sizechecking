{-# LANGUAGE NoMonomorphismRestriction #-}

module Tests.BindTest where

import Lambda
import SizedFun
import Exp
import Prelude ( ($), Int, (==), return, sequence, (>>=), and, (.), IO, Bool, String, const )
import qualified Control.Monad
import qualified Tests.SizeTest as S
import qualified Tests.ExpTest as E

addOne = bind "addone" S.testAddOne E.testAddOne

concat = bind "concat" S.testConcat
  (lam $ \l1 -> lam $ \l2 -> match l1 l2 $ \x xs -> cons x (concat `app` xs `app` l2))
