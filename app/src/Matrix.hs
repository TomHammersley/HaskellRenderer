-- 4D Matrix Library
-- TODO - Rewrite all this to be a lot more efficient. Very early code...2/
{-# LANGUAGE BangPatterns #-}

module Matrix where

import Vector

data Matrix = Matrix ![Double] deriving (Show, Read, Eq)

-- Just pass back the identity matrix
identity ::  Matrix
identity = Matrix [1, 0, 0, 0,   0, 1, 0, 0,   0, 0, 1, 0,   0, 0, 0, 1]

-- Multiply together two matrices
mul :: Matrix -> Matrix -> Matrix
mul (Matrix a) (Matrix b) = Matrix [sum [(a !! (i * 4 + k)) * (b !! (k * 4 + j)) | k <- [0..3]] | j <- [0..3], i <- [0..3]]

-- Need matrix inversion code

-- Vector * Matrix
transformVector :: Matrix -> Vector -> Vector
transformVector (Matrix mat) (Vector !x !y !z !w) = Vector x' y' z' w'
  where
    !vec = [x, y, z, w]
    !xvector = take 4 mat
    !yvector = take 4 (drop 4 mat)
    !zvector = take 4 (drop 8 mat)
    !wvector = take 4 (drop 12 mat)
    !x' = sum $ zipWith (*) vec xvector
    !y' = sum $ zipWith (*) vec yvector
    !z' = sum $ zipWith (*) vec zvector
    !w' = sum $ zipWith (*) vec wvector

-- Build a matrix from 4 vectors
buildMatrix :: Vector -> Vector -> Vector -> Vector -> Matrix
buildMatrix (Vector xx xy xz _) (Vector yx yy yz _) (Vector zx zy zz _) (Vector px py pz _) = Matrix ([xx, xy, xz] ++ [px] ++
                                                                                                      [yx, yy, yz] ++ [py] ++
                                                                                                      [zx, zy, zz] ++ [pz] ++
                                                                                                      [0, 0, 0, 1])
getTranslation :: Matrix -> Vector
{-# SPECIALIZE INLINE getTranslation :: Matrix -> Vector #-}
getTranslation (Matrix a) = Vector x y z 1
    where
      [!x, !y, !z, _] = drop 12 a

translationMatrix :: Double -> Double -> Double -> Matrix
translationMatrix !x !y !z = Matrix [1, 0, 0, 0,   0, 1, 0, 0,   0, 0, 1, 0,   x, y, z, 1]
translationMatrix' :: Vector -> Matrix
translationMatrix' (Vector !x !y !z _) = Matrix [1, 0, 0, 0,   0, 1, 0, 0,   0, 0, 1, 0,   x, y, z, 1]
