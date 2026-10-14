{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromJust, isNothing, fromMaybe)
import Control.Monad
import Data.List (group, sort, minimumBy, elemIndex, sortBy, findIndices, maximumBy, findIndex, foldl', scanl', elemIndices, find, groupBy, isPrefixOf, tails, transpose)
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as MUV
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Sequence as Seq
import Data.Sequence (Seq((:<|), (:|>)), (|>), (><))
import Control.Monad.ST (runST, ST, RealWorld)
import Data.STRef
import Text.Printf (printf)
import Data.Ord (comparing, Down (Down))
import qualified Data.IntMap.Strict as IM
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map as M
import qualified Data.Heap as H
import Data.Char (digitToInt, intToDigit, ord)
import Data.Bits (xor, (.&.), (.|.), shiftL, shiftR)
import qualified Data.Set as S
import qualified Data.IntSet as IS
import Debug.Trace (trace, traceShow, traceM, traceShowM)
import GHC.STRef (STRef(STRef))
import Data.Functor.Product (Product(Pair))
-- import qualified Data.STRef as SR
-- import qualified Data.IORef as IR

type Pair = (Int, Int)
type Triple = (Int, Int, Int)
type PPair = (Pair, Pair)
type Binary = [Bool]
type CGrid = V.Vector Char
type Vint = V.Vector Int

readInt :: BS.ByteString -> Int
readInt = fst . fromJust . BS.readInt

readInteger :: BS.ByteString -> Integer
readInteger = fst . fromJust . BS.readInteger

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

alp :: Char -> Int
alp x = HM.fromList (zip ['A' .. 'Z'] [0 .. ]) HM.! x

-- binSearch :: UV.Unbox a => UV.Vector a -> (a -> Bool) -> Pair
-- binSearch v p = (left, right)
--     where
--         mini = p $ UV.head v
--         maxi = p $ UV.last v
--         len = UV.length v

--         left = le (0, len-1)
--         right = re (0, len-1)

--         le :: Pair -> Int
--         le (l, r)
--             | mini = -1
--             | r - l <= 1 = l
--             | otherwise = if midVal
--                 then le (mid, r)
--                 else le (l, mid)
--                 where
--                     mid = div (l + r) 2
--                     midVal = p $ v UV.! mid

--         re :: Pair -> Int
--         re (l, r)
--             | maxi = len
--             | r - l <= 1 = r
--             | otherwise = if midVal
--                 then re (l, mid)
--                 else re (mid, r)
--                 where
--                     mid = div (l + r) 2
--                     midVal = p $ v UV.! mid

-- binSearch :: UV.Unbox a => UV.Vector a -> (a -> Int) -> Int -> (Pair, Pair)
-- binSearch v p k
--     | mini == GT = ((-1,-1), (-1,-1))
--     | maxi == LT = ((len,len), (len,len))
--     | otherwise = (left, right)
--         where
--             [mini, maxi] = map ((`compare` k) . p) [UV.head v, UV.last v]
--             len = UV.length v

--             left = le (0, len-1)
--             right = re (0, len-1)

--             le :: Pair -> Pair
--             le (l, r)
--                 | mini == EQ = (-1, 0)
--                 | r - l <= 1 = (l, r)
--                 | otherwise = if midVal < k
--                     then le (mid, r)
--                     else le (l, mid)
--                     where
--                         mid = div (l + r) 2
--                         midVal = p $ v UV.! mid

--             re :: Pair -> Pair
--             re (l, r)
--                 | maxi == EQ = (len-1, len)
--                 | r - l <= 1 = (l, r)
--                 | otherwise = if midVal > k
--                     then re (l, mid)
--                     else re (mid, r)
--                     where
--                         mid = div (l + r) 2
--                         midVal = p $ v UV.! mid

binSearch :: UV.Vector Int -> (Int -> Int) -> Int -> (Pair, Pair)
binSearch v p k
    | mini == GT = ((-1,-1), (-1,-1))
    | maxi == LT = ((len,len), (len,len))
    | otherwise = (left, right)
        where
            [mini, maxi] = map ((`compare` k) . p) [UV.head v, UV.last v]
            len = UV.length v

            left = le (0, len-1)
            right = re (0, len-1)

            le :: Pair -> Pair
            le (l, r)
                | mini == EQ = (-1, 0)
                | r - l <= 1 = (l, r)
                | otherwise = if midVal < k
                    then le (mid, r)
                    else le (l, mid)
                    where
                        mid = div (l + r) 2
                        midVal = p $ v UV.! mid

            re :: Pair -> Pair
            re (l, r)
                | maxi == EQ = (len-1, len)
                | r - l <= 1 = (l, r)
                | otherwise = if midVal > k
                    then re (l, mid)
                    else re (mid, r)
                    where
                        mid = div (l + r) 2
                        midVal = p $ v UV.! mid


-- Deque
data Deque s a = Deque {
    front :: STRef s Int,
    back :: STRef s Int,
    size :: Int,
    vec :: MUV.MVector s a
}

newDeque :: UV.Unbox a => Int -> ST s (Deque s a)
newDeque n = do
    f <- newSTRef 0
    b <- newSTRef 0
    v <- MUV.new n
    return $ Deque { front = f, back = b, size = n, vec = v }

-- last insert
enQue :: UV.Unbox a => a -> Deque s a -> ST s ()
enQue x Deque {..} = do
    b <- readSTRef back
    MUV.write vec (b `mod` size) x
    writeSTRef back (b + 1)

-- first insert
enQueFront :: UV.Unbox a => a -> Deque s a -> ST s ()
enQueFront x Deque {..} = do
    f <- readSTRef front
    MUV.write vec ((f - 1) `mod` size) x
    writeSTRef front (f - 1)

push :: UV.Unbox a => a -> Deque s a -> ST s ()
push = enQue

-- first out
deQue :: UV.Unbox a => Deque s a -> ST s a
deQue Deque {..} = do
    f <- readSTRef front
    x <- MUV.read vec (f `mod` size)
    writeSTRef front (f + 1)
    return x

deQue_ :: UV.Unbox a => Deque s a -> ST s ()
deQue_ Deque {..} = do
    f <- readSTRef front
    writeSTRef front (f + 1)

-- last out
pop :: UV.Unbox a => Deque s a -> ST s a
pop Deque {..} = do
    b <- readSTRef back
    x <- MUV.read vec ((b - 1) `mod` size)
    writeSTRef back (b - 1)
    return x

pop_ :: UV.Unbox a => Deque s a -> ST s ()
pop_ Deque {..} = do
    b <- readSTRef back
    writeSTRef back (b - 1)

isEmpty :: Deque s a -> ST s Bool
isEmpty Deque {..} = do
    f <- readSTRef front
    b <- readSTRef back
    return (f == b)

clear :: Deque s a -> ST s ()
clear Deque {..} = do
    writeSTRef front 0
    writeSTRef back 0

copyData :: UV.Unbox a => Deque s a -> ST s (UV.Vector a)
copyData Deque {..} = do
    f <- readSTRef front
    b <- readSTRef back
    let len = b - f
    v <- MUV.new len
    forM_ [0 .. len-1] $ \i -> do
        x <- MUV.read vec ((f + i) `mod` size)
        MUV.write v i x
    UV.freeze v

long :: UV.Unbox a => Deque s a -> ST s Int
long Deque {..} = do
    f <- readSTRef front
    b <- readSTRef back
    return (b - f)


--Segmented Tree
data SegTree s a = SegTree {
    segSize :: Int,
    segTree :: MUV.MVector s a,
    segOp :: a -> a -> a,
    segId :: a
}

newSegTree :: UV.Unbox a => (a -> a -> a) -> a -> [a] -> ST s (SegTree s a)
newSegTree op e xs = do
    let n = length xs
        size = 2 ^ (ceiling (logBase 2 (fromIntegral n)) :: Int)

    tree <- MUV.replicate (2 * size) e

    forM_ (zip [0 .. ] xs) $ \(i,x) -> MUV.write tree (size + i) x

    forM_ [size-1, size-2 .. 1] $ \i -> do
        l <- MUV.read tree (2*i)
        r <- MUV.read tree (2*i + 1)
        MUV.write tree i (op l r)

    return $ SegTree { segSize = size, segTree = tree, segOp = op, segId = e }

updateSegTree :: UV.Unbox a => SegTree s a -> Int -> a -> ST s ()
updateSegTree seg i x = do
    let tree = segTree seg
        size = segSize seg
        op = segOp seg

        go k
            | k == 1 = pure ()
            | otherwise = do
                let p = div k 2
                
                l <- MUV.read tree (2*p)
                r <- MUV.read tree (2*p+1)

                MUV.write tree p (op l r)

                go p

    let k = size + i

    MUV.write tree k x
    go k

-- get query [ql, qr)
querySegTree :: UV.Unbox a => SegTree s a -> Pair -> ST s a
querySegTree seg (ql, qr) = go 1 0 size
    where
        size = segSize seg
        tree = segTree seg
        op = segOp seg
        e = segId seg

        go node l r
            | qr <= l || r <= ql = pure e
            | ql <= l && r <= qr = MUV.read tree node
            |otherwise = do
                let m = div (l+r) 2

                x <- go (node * 2) l m
                y <- go (node * 2 + 1) m r

                pure $ op x y

-- CSR Graph
data Graph = Graph {
    adj :: UV.Vector Int,
    offset :: UV.Vector Int
}

newGraph :: V.Vector [Int] -> Graph
newGraph g = Graph {..}
    where
        adj = UV.fromList $ concat $ V.toList g
        offset = UV.fromList $ scanl' (+) 0 $ map length $ V.toList g

neighbors :: Graph -> Int -> UV.Vector Int
neighbors Graph {..} v = UV.slice l (r - l) adj
    where
        l = offset UV.! v
        r = offset UV.! (v + 1)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) f g x y = f $ g x y

(|>>) :: a -> (a -> b) -> b
x |>> f = f x

prefixIndices :: String -> String -> [Int]
prefixIndices s t = findIndices (isPrefixOf t) (tails s)

-- This is a template I use for AtCoder contests. The source code is available at https://github.com/pinattutaro/atcoder. -------------------------------------------------------------------------------------------------------------------

main :: IO()
main = putStrLn "hello haskell world!!"