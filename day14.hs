{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Lens (view)
import Control.Monad.State (State, execStateT, lift, gets, modify)
import Data.Array.ST
import Linear.V2
import qualified Data.Map as M

nspins :: Int
nspins = 1000000000

data Obj = None | Rock | Roll deriving (Eq,Ord,Show)

obj '.' = None
obj '#' = Rock
obj 'O' = Roll

type Loc = V2 Int
type Ary s = STArray s Loc Obj

main :: IO ()
main = do
    rows <- lines <$> getContents
    let nrows = length rows
    let ncols = length (head rows)
    print $ runST do
        ary <- newArray (V2 0 0, V2 (nrows+1) (ncols+1)) Rock :: ST s (Ary s)
        vv <- flip execStateT [] do
            for_ (zip [1..] rows) \(r,row) -> do
                for_ (zip [1..] row) \(c,x) -> do
                    let o = obj x
                    lift $ writeArray ary (V2 r c) o
                    when (o == Roll) (modify (V2 r c :))
        let f vv = do
                vv' <- spin ary vv
                x <- load ary vv'
                pure (x,vv')
        accel 0 f vv

accel :: (Monad m, Ord x, Show x) => Int -> (a -> m (x,a)) -> a -> m x
accel n f = go n M.empty where
    go n m a = do
        (x,a') <- f a
        let n' = n + 1
        let m' = M.insertWith (<>) x [n'] m
        let nn = take 4 $ (m' M.! x) <> [0,0,0]
        let (d:dd) = zipWith (-) nn (tail nn)
        if {- traceShow (n',x,nn,d:dd) -} (n' >= nspins) then
            pure x
        else if all (==d) dd then
            let r = rem (nspins-n') d in
            if r == 0 then pure x
            else go (nspins-r) m' a'
        else
            go (n+1) m' a'

slide :: Ary s -> Loc -> Loc -> ST s Loc
slide ary d v = do
    let w = v + d
    src <- readArray ary v
    dst <- readArray ary w
    if (dst == None && src == Roll) then do
        writeArray ary w Roll
        writeArray ary v None
        slide ary d w
    else pure v

north :: Ary s -> [Loc] -> ST s ()
north ary vv =
    for_ (sort vv) $ slide ary (V2 (-1) 0)

spin :: Ary s -> [Loc] -> ST s [Loc]
spin ary vv = do
    vv <- for (sortBy (comparing $ view _x) vv) $ slide ary $ V2 (-1) 0
    vv <- for (sortBy (comparing $ view _y) vv) $ slide ary $ V2 0 (-1)
    vv <- for (sortBy (comparing $ Down . view _x) vv) $ slide ary $ V2 1 0
    vv <- for (sortBy (comparing $ Down . view _y) vv) $ slide ary $ V2 0 1
    pure vv

load :: Ary s -> [Loc] -> ST s Int
load ary vv = do
    (_, V2 rhi _) <- getBounds ary
    pure . sum $ f rhi <$> vv
  where
    f hi v = hi - view _x v
