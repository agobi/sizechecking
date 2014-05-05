{-# LANGUAGE TypeFamilies, GADTs, Rank2Types, TemplateHaskell #-}

module Lambda where

import qualified Data.Supply as S
import qualified Data.Char as C
import Data.Lens.Light 

{-
 - Lambda calculus beagyazas
 -}
class Lambda l where
    lam :: (l a -> l b) -> l (a -> b)
    app :: l (a -> b) -> l a -> l b
    lit :: Int -> l Int

{-
 - eval interpreter
 -}
newtype Q a = Q { unQ :: a }
instance Lambda Q where
    lit = Q
    lam a = Q (unQ.a.Q)
    app a b = Q $ unQ a (unQ b)

eval :: Q a -> a
eval = unQ

{-
- show interpreter
-}
showVar :: Int -> String -> String
showVar x = if x>28 
    then showVar (x `div` 29) . showChar (C.chr $ C.ord 'a' + (x `mod` 29))
    else showChar $ C.chr $ C.ord 'a' + x

class SContext a where
  supply :: Lens a (S.Supply Int)
  prec :: Lens a Int

newtype S ctx a = S { unS :: ctx -> ShowS }

instance SContext ctx => Lambda (S ctx) where
    lit a = S (\ctx -> showsPrec (getL prec ctx) a)
    app (S fun) (S arg) = S (\ctx ->
        let (s1, s2) = S.split2 (getL supply ctx)
            p = getL prec ctx
        in showParen (p>6) $ fun (updateCtx s1 6 ctx) . showChar ' ' . arg (updateCtx s2 7 ctx))
    lam fun = S (\ctx -> 
        let (s1, s2) = S.split2 (getL supply ctx)
            v        = S.supplyValue s1
            p        = getL prec ctx
            showV = S $ \ctx -> showVar v
        in showParen (p>0) $ showChar 'λ' . showVar v . showChar '.' . unS (fun showV) (updateCtx s2 0 ctx))

updateCtx :: SContext ctx => S.Supply Int -> Int -> ctx -> ctx
updateCtx s p = setL supply s . setL prec p

data SData = SData { _getSDataSupply :: S.Supply Int, _getSDataPrec :: Int }
$(makeLens ''SData)

instance SContext SData where
  supply = getSDataSupply
  prec = getSDataPrec

ast :: S SData a -> IO ShowS
ast a = do
    s <- S.newSupply 0 (+1)
    return $ unS a $ SData s 0

printAst :: S SData a -> IO ()
printAst l = ast l >>= (\s -> putStrLn $ s "")

