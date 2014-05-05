{-# LANGUAGE TypeFamilies,TemplateHaskell,MultiParamTypeClasses,FlexibleContexts,FlexibleInstances,OverlappingInstances,IncoherentInstances #-}
{-# LANGUAGE GADTs #-}

module SizedFun where

import Lambda
import Size
import Exp
import Ops

import qualified Data.Supply as S
import Data.Lens.Light

class Infer a b where
instance (Infer a b, Infer p q) => Infer (a->p) (b->q)
instance Infer a b => Infer [a] [b]
instance Infer a a 
instance Infer Unsized Int

class (Exp e, Size (SizeExp e)) => SizedFun e where
  type SizeExp e :: * -> *
  bind :: Infer a b => String -> SizeExp e a -> e b -> e b

class SContext s => SBContext s where
    bound :: Lens s Bool

instance SBContext s => SizedFun (S s) where
  type SizeExp (S s) = S s
  bind name size exp = S $ \ctxo -> if getL bound ctxo then
      showString name
      else let (s1, s2) = S.split2 (getL supply ctxo)
               ctx = setL bound True ctxo
      in showString name . showString " :: " . unS size (updateCtx s1 0 ctx) . showChar '\n'.
         showString name . showString " = "  . unS exp (updateCtx s2 0 ctx) 

data SBData = SBData { _getSBDataSupply :: S.Supply Int, _getSBDataPrec :: Int, _getSBDataBound :: Bool }
$(makeLens ''SBData)

instance SContext SBData where
    supply = getSBDataSupply
    prec = getSBDataPrec

instance SBContext SBData where
    bound = getSBDataBound

astf :: S SBData a -> IO ShowS
astf a = do
    s <- S.newSupply 0 (Prelude.+1)
    return $ unS a $ SBData s 0 False

printFun :: S SBData a -> IO ()
printFun l = astf l >>= (\s -> putStrLn $ s "")

instance SizedFun Q where
    type SizeExp Q = S SBData
    bind name size exp = exp

data DeclSize b where
  DeclSize :: Infer a b => S SData a -> DeclSize b

instance Lambda DeclSize where
instance LOps DeclSize where
instance Exp DeclSize where
instance SizedFun DeclSize where
    type SizeExp DeclSize = S SData
    bind name size exp = DeclSize size

getDeclSize (DeclSize size) = size
