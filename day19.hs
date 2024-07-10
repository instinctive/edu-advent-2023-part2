{-# LANGUAGE TemplateHaskell #-}

module Main where

import Prelude hiding (some,try,inRange)

import Control.Lens
import Data.Array ((!), Array, listArray)
import Data.Map (Map)
import Data.Text (Text)
import Text.Megaparsec hiding (parse)
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
inRange x (Range lo hi) = lo <= x && x < hi
subtractRange orig@(Range lo hi) (Range xlo xhi)
    | xlo >= hi = [orig]
    | xhi <= lo = [orig]
    | xlo <= lo && xhi >= hi = []
    | xlo <= lo = [orig & hiRange .~ xhi]
    | xhi >= hi = [orig & loRange .~ xlo]
    | otherwise = [orig & hiRange .~ xlo, orig & loRange .~ xhi]
isSubRange (Range a b) (Range u v) = a >= u && b <= v
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
inBounds (x,m,a,s) (Bounds xx mm aa ss) =
    inRange x xx &&
    inRange m mm &&
    inRange a aa &&
    inRange s ss
subtractBounds (Bounds a b c d) (Bounds w x y z) =
    Bounds <$> subtractRange a w
           <*> subtractRange b x
           <*> subtractRange c y
           <*> subtractRange d z
isSubBounds (Bounds a b c d) (Bounds w x y z) =
    isSubRange a w &&
    isSubRange b x &&
    isSubRange c y &&
    isSubRange d z
-- }}}

test :: Xmas -> Ordering -> Int -> (Bounds, Bounds) -- {{{
test x o v = case o of
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

type Ans = Map Text [Bounds]

main = do
    flows <- getContents <&> map parse . takeWhile (/="") . lines
    let m = M.fromList $ second (validFlow m) <$> flows
    let bb = m M.! (T.pack "in")
    -- traverse_ print bb
    print $ sum $ map countBounds $ filter (pred bb) $
        Bounds <$> ranges bb xBound
               <*> ranges bb mBound
               <*> ranges bb aBound
               <*> ranges bb sBound
  where
    pred bb b = any (isSubBounds b) bb

ranges bb l = zipWith Range vv (tail vv) where
    vv = IS.toList . IS.fromList $
        (bb ^.. each.l.loRange) <>
        (bb ^.. each.l.hiRange)

isect :: Bounds -> Bounds -> Maybe Bounds
isect a b = a <> b & bool Nothing . Just <*> checkBounds

validName :: Ans -> Name -> [Bounds]
-- validName m q | traceShow ("validName",q) False = undefined
validName m Accept = [allBounds]
validName m Reject = []
validName m (Text text) = m M.! text

validFlow :: Ans -> Flow -> [Bounds]
-- validFlow m qq | traceShow qq False = undefined
validFlow m [Name name] = validName m name
validFlow m (Test x o v name : more) = catMaybes $
    ( isect succ <$> validName m name ) <>
    ( isect fail <$> validFlow m more )
  where
    (succ,fail) = test x o v
