module Main where

import Control.Monad.State (State, evalState, gets, modify)
import Data.Function (fix)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M

main :: IO ()
main = do
    lines <- lines <$> getContents
    print $ sum $ solve . parse 5 <$> lines

type Input = (String,[Int])

parse :: Int -> String -> Input
parse n (words -> [ss, dd']) = quux n (ss,dd)
  where
    dd = read @Int <$> splitOn "," dd'

quux n (ss,dd) =
    ( intercalate "?" $ replicate n ss
    , concat $ replicate n dd )

-- solve input = fix (memoize input . solve') input

-- memoize (ss,dd) f = (m M.!)
--   where
--     m = M.fromList $ mk <$> inputs
--     mk i = (i, f i)
--     inputs = (,) <$> tails ss <*> tails dd

solve :: Input -> Int
solve input =
    flip evalState M.empty $ memo input
  where
    memo :: Input -> State (Map Input Int) Int
    memo input = gets (M.lookup input) >>= \case
        Just v -> pure v
        Nothing -> do
            v <- uncurry go input
            modify (M.insert input v)
            pure v

    go "" [0] = pure 1
    go (c:ss) (0:dd)
        | c == '#' = pure 0
        | otherwise = memo (ss,dd)
    go ('.':ss) dd = memo (ss,dd)
    go ss []
        | any (=='#') ss = pure 0
        | otherwise = pure 1
    go "" _ = pure 0
    go ('?':ss) dd = liftA2 (+) (memo (ss,dd)) (memo ('#':ss,dd))
    go ss@('#':_) (d:dd)
        | length xx == d && all (/='.') xx = memo (yy,0:dd)
        | otherwise = pure 0
      where
        (xx,yy) = splitAt d ss

-- solve' :: (Input -> Int) -> Input -> Int
-- solve' f (ss,dd) = go ss dd where
--     go "" [0] = 1
--     go (c:ss) (0:dd)
--         | c == '#' = 0
--         | otherwise = f (ss,dd)
--     go ('.':ss) dd = f (ss,dd)
--     go ss []
--         | any (=='#') ss = 0
--         | otherwise = 1
--     go "" _ = 0
--     go ('?':ss) dd = f (ss,dd) + f ('#':ss,dd)
--     go ss@('#':_) (d:dd)
--         | length xx == d && all (/='.') xx = f (yy,0:dd)
--         | otherwise = 0
--       where
--         (xx,yy) = splitAt d ss
