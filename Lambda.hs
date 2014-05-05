{-# LANGUAGE TypeFamilies, GADTs, Rank2Types #-}

module Lambda where

import qualified Data.Supply as S
import qualified Data.Char as C
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (newIORef, readIORef, writeIORef)

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
  supply :: a -> S.Supply Int 
  prec :: a -> Int 

newtype S ctx a = S { unS :: ctx -> ShowS }

instance SContext ctx => Lambda (S ctx) where
    lit a = S (\ctx -> showsPrec (prec ctx) a)
    app (S fun) (S arg) = S (\ctx -> 
        let (s1, s2) = S.split2 s 
        in showParen (p>6) $ fun s1 6 . showChar ' ' . arg s2 7)
    lam fun = S (\s p -> 
        let (s1, s2) = S.split2 s
            v        = S.supplyValue s1
            showV = S $ \_ _ -> showVar v
        in showParen (p>0) $ showChar 'λ' . showVar v . showChar '.' . unS (fun showV) s2 0)

data SData = SData { getSDataSupply :: S.Supply Int,  getSDataPrec :: Int }

instance SContext SData where
  supply = getSDataSupply
  prec = getSDataPrec

ast :: S a -> IO ShowS
ast a = do
    s <- S.newSupply 0 (+1)
    return $ unS a $ SData s 0

printAst :: S a -> IO ()
printAst l = ast l >>= (\s -> putStrLn $ s "")

