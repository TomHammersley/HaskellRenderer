-- 4D Matrix Library
{-# LANGUAGE BangPatterns #-}

module Matrix where

import Vector

data Matrix = Matrix ![Double] deriving (Show, Read, Eq)

-- Just pass back the identity matrix
identity ::  Matrix
identity = Matrix [1, 0, 0, 0,   0, 1, 0, 0,   0, 0, 1, 0,   0, 0, 0, 1]

-- This multiplies one element of the matrix against
mulElement :: Matrix -> Matrix -> Int -> Int -> Double
mulElement (Matrix mat1) (Matrix mat2) i j = sum [(mat1 !! (i * 4 + k)) * (mat2 !! (k * 4 + j)) | k <- [0..3]]

-- Multiply together two matrices
-- TODO - Turn mulElement into a lambda function?
mul :: Matrix -> Matrix -> Matrix
mul a b = Matrix [mulElement a b i j | j <- [0..3], i <- [0..3]]

-- Need matrix inversion code

-- Vector * Matrix
transformVector :: Matrix -> Vector -> Vector
transformVector (Matrix mat) (Vector x y z w) = Vector (sum $ zipWith (*) vec xvector) (sum $ zipWith (*) vec yvector) (sum $ zipWith (*) vec zvector) (sum $ zipWith (*) vec wvector)
                          where
                            vec = [x, y, z, w]
                            xvector = take 4 mat
                            yvector = take 4 (drop 4 mat)
                            zvector = take 4 (drop 8 mat)
                            wvector = take 4 (drop 12 mat)

-- Build a matrix from 4 vectors
buildMatrix :: Vector -> Vector -> Vector -> Vector -> Matrix
buildMatrix (Vector xx xy xz _) (Vector yx yy yz _) (Vector zx zy zz _) (Vector px py pz _) = Matrix ([xx, xy, xz] ++ [px] ++
                                                                                                      [yx, yy, yz] ++ [py] ++
                                                                                                      [zx, zy, zz] ++ [pz] ++
                                                                                                      [0, 0, 0, 1])
getTranslation :: Matrix -> Vector
{-# SPECIALIZE INLINE getTranslation :: Matrix -> Vector #-}
--getTranslation (Matrix [_, _, _, _, _, _, _, _, _, _, _, _, !x, !y, !z, !w]) = Vector x y z w
--getTranslation (Matrix _) = error "Matrix is wrong size"
getTranslation (Matrix a) = Vector x y z 1
    where
      [!x, !y, !z, _] = drop 12 a

translationMatrix :: Double -> Double -> Double -> Matrix
translationMatrix x y z = Matrix [1, 0, 0, 0,   0, 1, 0, 0,   0, 0, 1, 0,   x, y, z, 1]
translationMatrix' :: Vector -> Matrix
translationMatrix' (Vector x y z _) = Matrix [1, 0, 0, 0,   0, 1, 0, 0,   0, 0, 1, 0,   x, y, z, 1]