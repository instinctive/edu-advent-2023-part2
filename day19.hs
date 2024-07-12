{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (some,try,inRange)

import Control.Lens
import Data.Array ((!), Array, listArray)
import Data.Map (Map)
import Data.Text (Text)
import Text.Megaparsec hiding (parse, count)
import Text.Megaparsec.Char
import qualified Data.Map as M
import qualified Data.IntSet as IS
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

data Xmas = X | M | A | S deriving (Eq,Ord,Bounded,Ix,Show)
data Name = Text Text | Accept | Reject deriving (Eq,Ord,Show)
data Rule = Name Name | Test Xmas Ordering Int Name deriving Show
type Flow = [Rule]

data Range = Range { _loRange, _hiRange :: !Int } -- {{{
makeLenses ''Range
instance Show Range where
    show (Range lo hi) = printf "[%d..%d]" lo (hi-1)
instance Semigroup Range where
    Range a b <> Range x y = Range (max a x) (min b y)
allRange = Range 1 4001
checkRange (Range lo hi) = lo < hi
countRange (Range lo hi) = max 0 (hi - lo)
-- }}}

data Bounds = Bounds { _xBound, _mBound, _aBound, _sBound :: !Range } -- {{{
makeLenses ''Bounds
instance Show Bounds where
    show (Bounds x m a s) = printf "{x:%s,m:%s,a:%s,s:%s}" (show x) (show m) (show a) (show s)
instance Semigroup Bounds where
    Bounds a b c d <> Bounds w x y z = Bounds (a <> w) (b <> x) (c <> y) (d <> z)
instance Monoid Bounds where
    mempty = allBounds
allBounds = Bounds allRange allRange allRange allRange
checkBounds (Bounds x m a s) = checkRange x && checkRange m && checkRange a && checkRange s
countBounds (Bounds x m a s) = countRange x * countRange m * countRange a * countRange s
splitBounds l v b
    | b^.runLens l.loRange >= v = ([],[b])
    | b^.runLens l.hiRange <= v = ([b],[])
    | otherwise = ([b & runLens l.hiRange .~ v],[b & runLens l.loRange .~ v])
-- }}}

runTest :: Xmas -> Ordering -> Int -> (Bounds, Bounds) -- {{{
runTest x o v = case o of
    LT -> ( allBounds & l x . hiRange .~ v
          , allBounds & l x . loRange .~ v )
    GT -> ( allBounds & l x . loRange .~ v+1
          , allBounds & l x . hiRange .~ v+1 )
  where
    l X = xBound
    l M = mBound
    l A = aBound
    l S = sBound
-- }}}

parse :: String -> (Text,Flow) -- {{{
parse = check . runParser pFlow "foo!" where
    check (Right a) = a
    check (Left e) = error $ errorBundlePretty e

type Parser = Parsec Void String

pFlow :: Parser (Text,[Rule])
pFlow = do
    name <- some alphaNumChar
    rules <- between (char '{') (char '}') (sepBy1 pRule (char ','))
    pure (T.pack name,rules)

pName :: Parser Name
pName =
    (char 'A' <&> const Accept) <|>
    (char 'R' <&> const Reject) <|>
    (some alphaNumChar <&> Text . T.pack)

pOp :: Parser Ordering
pOp = satisfy (flip elem "<>") <&> \case
    '<' -> LT
    '>' -> GT

pXmas :: Parser Xmas
pXmas = satisfy (flip elem "xmas") <&> \case
    'x' -> X
    'm' -> M
    'a' -> A
    's' -> S

pRule :: Parser Rule
pRule = try cond <|> fmap Name pName where
    cond = do
        k <- pXmas
        op <- pOp
        v <- L.decimal
        char ':'
        l <- pName
        pure $ Test k op v l
-- }}}

main = do
    flows <- getContents <&> map parse . takeWhile (/="") . lines
    let m = M.fromList $ second (validFlow m) <$> flows
    let bb = m M.! (T.pack "in")
    print $ sum $ countBounds <$> bb
    -- not necessary as there can be no overlaps at this point!
    -- print $ count bb
    --     [ values bb $ Lens xBound
    --     , values bb $ Lens mBound
    --     , values bb $ Lens aBound
    --     , values bb $ Lens sBound
    --     ]

count [] _ = 0
count [b] _ = countBounds b
count bb ((_,[_,_]):rr) = count bb rr
count bb ((l,vv):rr) =
    count bblo (rr <> [(l,xx <> [w])]) +
    count bbhi (rr <> [(l,w:yy)])
  where
    (bblo,bbhi) = bimap concat concat $ unzip $ splitBounds l w <$> bb
    (xx,w:yy) = splitAt (length vv `div` 2) vv

values bb l = (l, vv) where
    vv = IS.toList . IS.fromList $
        (bb ^.. each.runLens l.loRange) <>
        (bb ^.. each.runLens l.hiRange)

isect :: Bounds -> Bounds -> Maybe Bounds
isect a b = a <> b & bool Nothing . Just <*> checkBounds

validName m Accept = [allBounds]
validName m Reject = []
validName m (Text text) = m M.! text

validFlow m [Name name] = validName m name
validFlow m (Test x o v name : more) = catMaybes $
    ( isect succ <$> validName m name ) <>
    ( isect fail <$> validFlow m more )
  where
    (succ,fail) = runTest x o v
