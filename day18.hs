{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Set (Set)
import Linear
import qualified Data.Set as S

data Dir = U | R | D | L deriving (Eq,Ord,Enum,Show,Read)
data Turn = Rt | Lt deriving (Eq,Ord,Show)
type V = V2 Int
type Step = (Dir,Int)

turn a b = bool Lt Rt $ mod (fromEnum b - fromEnum a) 4 == 1

data Vert = Vert { _vtx :: V, _interp :: Bool } deriving (Eq,Ord)
makeLenses ''Vert
vtxx = vtx . _x
vtxy = vtx . _y
instance Show Vert where
    show (Vert (V2 x y) i) = printf "(%d %d)%s" x y (bool "" "*" i)

delta :: Dir -> V -- {{{
-- working in standard x/y coordinate space
delta U = V2 0 1
delta R = V2 1 0
delta D = V2 0 (-1)
delta L = V2 (-1) 0
-- }}}

main :: IO ()
main = do
    (part1,part2) <- getContents <&> unzip . map parse . lines
    print $ solve part1
    print $ solve part2

parse :: String -> (Step,Step) -- {{{
parse s = (part1,part2) where
    [dir1,len,rgb] = words s
    part1 = (read @Dir dir1, read @Int len)
    (hex,[dir2]) = splitAt 5 . take 6 . drop 2 $ rgb
    part2 = (part2dir dir2, read @Int $ "0x" <> hex)
    part2dir '0' = R
    part2dir '1' = D
    part2dir '2' = L
    part2dir '3' = U
-- }}}

solve :: [Step] -> Int -- {{{
solve = sum . unfoldr takeBox . vertices
-- }}}

vertices :: [Step] -> [Vert] -- {{{
vertices (unzip -> (dd,nn)) =
    -- traceShow (map (flip Vert False) vv) $
    verts
  where
    tt = zipWith turn dd (tail dd <> [head dd])
    aa = zipWith f ([last tt] <> tt) tt where
        -- assumes CW (#Rt == #Lt + 4)
        f Rt Rt =  1
        f Lt Lt = -1
        f _ _   =  0
    vv = tail $ flip scanl 0 f (zip dd $ zipWith (+) aa nn) where
        f p (d,n) = p + delta d ^* n
    verts = S.toList . S.fromList $ map (flip Vert False) vv <> map (flip Vert True) ii
    xx = S.elems $ S.fromList $ view _x <$> vv
    hh = zip vv (tail vv <> [head vv]) & filter horz where horz (a,b) = a^._y == b^._y
    ii = concat $ mk <$> hh <*> xx where
        mk (a,b) x | a^._x > b^._x = mk (b,a) x
        mk (a,b) x = bool [] [set _x x a] (a^._x < x && x < b^._x)
-- }}}

takeBox :: [Vert] -> Maybe (Int, [Vert]) -- {{{
takeBox [] = Nothing
takeBox qq@(bl:tl:vv0) =
    -- traceShow ("qq",qq) $
    -- traceShow ("box",area,bl,tl,tr,br) $
    Just (area, vv)
  where
    area = width * height -- + extra
    width  = tr^.vtxx - tl^.vtxx
    height = tl^.vtxy - bl^.vtxy
    Just (vv1,br:vv2) = findIndex ((==bl^.vtxy).view vtxy) vv0 <&> flip splitAt vv0
    Just (vv3,tr:vv4) = findIndex ((==tl^.vtxy).view vtxy) vv2 <&> flip splitAt vv2
    vv = vv1 <> xorg br <> vv3 <> xorg tr <> vv4
    xorg vert@Vert{..} = bool [] [vert] _interp
-- }}}
