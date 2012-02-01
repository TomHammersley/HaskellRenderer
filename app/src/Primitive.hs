-- Module for general primitives and intersections

module Primitive (primitiveBoundingRadius, 
                  primitiveClosestIntersect, 
                  primitiveAnyIntersect,
                  Object(Object), 
                  Primitive(Sphere, Plane, TriangleMesh, Box, SparseOctreeModel), 
                  primitive, 
                  material, 
                  makeQuad, 
                  quadsToTriangles,
                  vertPosition, 
                  vertUV, 
                  vertTangentSpace, 
                  transform, 
                  radius,
                  triangles, 
                  getCentre, 
                  planeDistance, 
                  primitiveBoundingBox, 
                  objectListBoundingBox, 
                  intersectsBox, 
                  infinite, 
                  boundingBoxValid, 
                  sphereIntersect,
                  makePlane,
                  intersectRayTriangle,
                  TangentSpace,
                  Vertex,
                  bounds,
                  svo) where

import PolymorphicNum
import Ray
import Vector
import Material
import Matrix
import BoundingBox
import Data.Maybe
import Data.List
import {-# SOURCE #-} SparseVoxelOctree

-- Triangle object used for triangle meshes
data Vertex = Vertex { vertPosition :: {-# UNPACK #-} !Position, 
                       vertUV :: {-# UNPACK #-} !Position, 
                       vertTangentSpace :: {-# UNPACK #-} !TangentSpace } deriving (Show, Eq)
data Triangle = Triangle { vertices :: ![Vertex], plane :: !Primitive, halfPlanes :: ![Primitive] } deriving (Show, Eq)

-- General object definition
data Object = Object { primitive :: Primitive,
                       material :: Material,
                       transform :: !Matrix} deriving (Show, Eq)

-- Different kinds of primitives that an object can have
data Primitive = Sphere { radius :: {-# UNPACK #-} !Double }
               | Plane { planeTangentSpace :: {-# UNPACK #-} !TangentSpace, planeDistance :: {-# UNPACK #-} !Double }
               | TriangleMesh { triangles :: [Triangle] } 
               | Box { bounds :: {-# UNPACK #-} !AABB } 
               | SparseOctreeModel { svo :: SparseOctree } deriving (Show, Eq)

-- Get the centre of an object
getCentre :: Object -> Vector
getCentre object = getTranslation $ transform object

-- Surface normal for 3 points
surfaceNormal :: Position -> Position -> Position -> Direction
surfaceNormal v1 v2 v3 = (v2 <-> v1) `cross` (v3 <-> v1)

-- Make a plane
makePlane :: Position -> Position -> Position -> Primitive
makePlane v1 v2 v3 = Plane (tangent, binormal, normal) (-(v1 `dot3` normal))
    where 
      normal = normalise (surfaceNormal v1 v2 v3)
      tangent = normalise (v2 <-> v1)
      binormal = normalise (normal `cross` tangent)

makePlaneWithTangents :: Position -> Position -> Position -> Direction -> Direction -> Primitive
makePlaneWithTangents v1 v2 v3 tangent binormal = Plane (tangent, binormal, normal) (-(v1 `dot3` normal))
    where 
      normal = normalise (surfaceNormal v1 v2 v3)

-- -------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Triangle base functionality

-- Make a triangle
makeTriangleWithTangents :: Position -> Position -> Position -> Direction -> Direction -> Triangle
makeTriangleWithTangents v1 v2 v3 tangent binormal = Triangle verts newPlane newHalfPlanes
    where newPlane = makePlaneWithTangents v1 v2 v3 tangent binormal
          newTanSpace = planeTangentSpace newPlane
          verts = map (\v -> Vertex v zeroVector newTanSpace) [v1, v2, v3]
          edgeVertices = [v1, v2, v3]
          edges = map normalise [v2 <-> v1, v3 <-> v2, v1 <-> v3]
          edgeNormals = map (\edge -> normalise (tsNormal newTanSpace `cross` edge)) edges
          newHalfPlanes = zipWith3 (\edgeNormal edgeVertex edgeDir -> Plane (edgeDir, tsNormal newTanSpace, edgeNormal) (-(edgeNormal `dot3` edgeVertex))) edgeNormals edgeVertices edges

makeQuad :: [Position] -> [Triangle]
makeQuad [vert1, vert2, vert3, vert4] = [makeTriangleWithTangents vert1 vert2 vert3 tangent binormal, 
                                         makeTriangleWithTangents vert1 vert3 vert4 tangent binormal]
    where
      tangent = normalise (vert2 <-> vert1)
      binormal = normalise (vert3 <-> vert1)
makeQuad _ = error "makeQuad: List was invalid size"

-- Turn a list of quad vertices into a triangle list
quadsToTriangles :: [Position] -> [Triangle]
quadsToTriangles positions = quadsToTriangles' positions []
    where
      quadsToTriangles' verts currentList 
          | length verts >= 4 = quadsToTriangles' (drop 4 verts) (currentList ++ makeQuad (take 4 verts))
          | otherwise = currentList

-- Area of a triangle
triangleArea :: Position -> Position -> Position -> Double
triangleArea v1 v2 v3 = 0.5 * magnitude (surfaceNormal v1 v2 v3)

-- Calculate the barycentric co-ordinates of a point on a triangle
calculateBarycentricCoordinates :: Position -> Triangle -> (Double, Double, Double)
calculateBarycentricCoordinates pos triangle = (alpha, beta, gamma)
    where alpha = triangleArea pos v2 v3 / area
          beta = triangleArea pos v1 v3 / area
          gamma = 1 - alpha - beta
          area = triangleArea v1 v2 v3
          [v1, v2, v3] = map vertPosition (vertices triangle)

-- Distance to a plane
distanceToPlane :: Primitive -> Vector -> Double
distanceToPlane (Plane (_, _, norm) dist) pos = (pos `dot3` norm) + dist
distanceToPlane _ _ = error "distanceToPlane: Unsupported primitive for this function"

-- Use halfplanes to test if a point is inside a triangle
pointInsideTriangle :: Triangle -> Position -> Bool
pointInsideTriangle tri point = all (\pln -> distanceToPlane pln point >= 0) (halfPlanes tri)

{-
pointInsideTriangleBary :: Triangle -> Position -> Bool
pointInsideTriangleBary tri point = u >= 0 && v >= 0 && (u + v) < 1
    where
      [a, b, c] = map vertPosition (vertices tri)
      v0 = c <-> a
      v1 = b <-> a
      v2 = point <-> a

      dot00 = v0 `dot3` v0
      dot01 = v0 `dot3` v1
      dot02 = v0 `dot3` v2

      dot11 = v1 `dot3` v1
      dot12 = v1 `dot3` v2

      invDenom = 1 / (dot00 * dot11 - dot01 * dot01)
      u = (dot11 * dot02 - dot01 * dot12) * invDenom
      v = (dot00 * dot12 - dot01 * dot02) * invDenom
-}

-- Intersect a ray with a triangle
intersectRayTriangle :: Ray -> Triangle -> Bool -> (Bool, Double, Triangle)
intersectRayTriangle ray triangle doubleSided
    | not doubleSided && direction ray `dot3` (tsNormal . planeTangentSpace . plane) triangle >= 0 = (False, 0, triangle)
    | otherwise = case planeIntersect (plane triangle) ray of
                    Nothing -> (False, 0, triangle)
                    Just (dist', _) -> if pointInsideTriangle triangle (pointAlongRay ray dist')
                                       then (True, dist', triangle)
                                       else (False, 0, triangle)

-- Intersect against a list of triangles
intersectRayTriangleList :: [Triangle] -> Int -> Maybe (Double, TangentSpace) -> Ray -> Object -> Maybe (Double, TangentSpace)
intersectRayTriangleList (x:xs) index acc ray obj = intersectRayTriangleList xs (index + 1) acc' ray' obj
    where
      (ray', acc') = case intersectRayTriangle ray x False of
                       (False, _, _) -> (ray, acc)
                       (True, dist, _) -> let intersectionPoint = pointAlongRay ray dist
                                              (triAlpha, triBeta, triGamma) = calculateBarycentricCoordinates intersectionPoint x
                                          in (shortenRay ray dist, Just (dist, interpolatedTangentSpace x triAlpha triBeta triGamma))
intersectRayTriangleList [] _ acc _ _ = acc

-- Intersect against any triangle
intersectRayAnyTriangleList :: [Triangle] -> Int  -> Ray -> Object -> Maybe (Double, TangentSpace)
intersectRayAnyTriangleList (x:xs) index ray obj = case intersectRayTriangle ray x False of
                                                     (False, _, _) -> intersectRayAnyTriangleList xs (index + 1) ray obj
                                                     (True, dist, _) -> let intersectionPoint = pointAlongRay ray dist
                                                                            (triAlpha, triBeta, triGamma) = calculateBarycentricCoordinates intersectionPoint x
                                                                        in Just (dist, interpolatedTangentSpace x triAlpha triBeta triGamma)
intersectRayAnyTriangleList [] _ _ _ = Nothing

-- Get the interpolated vertex normal
interpolatedTangentSpace :: Triangle -> Double -> Double -> Double -> TangentSpace
interpolatedTangentSpace triangle triAlpha triBeta triGamma = (tangent, binormal, normal)
    where [(tan1, bi1, norm1), (tan2, bi2, norm2), (tan3, bi3, norm3)] = map vertTangentSpace (vertices triangle)
          tangent = normalise $ tan1 <*> triAlpha <+> tan2 <*> triBeta <+> tan3 <*> triGamma
          binormal = normalise $ bi1 <*> triAlpha <+> bi2 <*> triBeta <+> bi3 <*> triGamma
          normal = normalise $ norm1 <*> triAlpha <+> norm2 <*> triBeta <+> norm3 <*> triGamma

planeIntersect :: Primitive -> Ray -> Maybe (Double, TangentSpace)
planeIntersect (Plane planeTanSpace@(_, _, planeNormal) planeD) (Ray rayOrg rayDir _ rayLen)
    | dirDotNormal == 0 = Nothing
    | intercept >= 0 && intercept <= rayLen = Just (intercept, planeTanSpace)
    | otherwise = Nothing
    where dirDotNormal = rayDir `dot3` planeNormal
          intercept = ((-planeD) - (rayOrg `dot3` planeNormal)) / dirDotNormal
planeIntersect _ _ = error "planeIntersect was called for an inappropriate primitive type" -- only available for this case

-- -------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Family of intersection functions
primitiveClosestIntersect :: Primitive -> Ray -> Object -> Maybe (Double, TangentSpace)

-- This function intersects a ray with a sphere, and returns the closest intercept
primitiveClosestIntersect (Sphere sphereRadius) ray@(Ray rayOrg rayDir _ rayLen) obj
    | discriminant < 0 = Nothing
    | discriminant == 0 = Just ((-b) ** (0.5 :: Double), tanSpace $ (-b) ** (0.5 :: Double))
    | root1 >= 0 && root1 <= rayLen = Just (root1, tanSpace root1)
    | root2 >= 0 && root2 <= rayLen = Just (root2, tanSpace root2)
    | otherwise = Nothing
    where 
      delta = rayOrg <-> getCentre obj
      b = 2 * (delta `dot3` rayDir)
      c = (delta `dot3` delta) - sphereRadius ** 2
      discriminant = b ** 2 - 4 * c -- A is 1 because the ray direction is normalised
      root1 = ((-b) - sqrt discriminant) * 0.5
      root2 = ((-b) + sqrt discriminant) * 0.5
      tanSpace d = let tangent = Vector 1 0 0 0 -- TODO - This is clearly incorrect - fix this later!
                       binormal = Vector 0 1 0 0
                       normal = (pointAlongRay ray d <-> getCentre obj) <*> (1 / sphereRadius)
                   in (tangent, binormal, normal)

-- This function intersects a ray with a plane and returns the closest intercept
primitiveClosestIntersect pln@(Plane (_, _, _) _) ray _ = planeIntersect pln ray

-- Find intersection with a triangle mesh
primitiveClosestIntersect (TriangleMesh tris) ray obj = intersectRayTriangleList tris 0 Nothing ray obj

-- TODO Need to transform ray by inverse object matrix
primitiveClosestIntersect (Box aabb) ray _ = case boundingBoxIntersectRay aabb ray of Nothing -> Nothing
                                                                                      Just (d, _) -> Just (d, boundingBoxTangentSpace aabb (pointAlongRay ray d))

-- TODO Need to transform ray by inverse object matrix
primitiveClosestIntersect (SparseOctreeModel svo') ray _ = SparseVoxelOctree.closestIntersect ray 0 50 svo' 

primitiveAnyIntersect :: Primitive -> Ray -> Object -> Maybe (Double, TangentSpace)
primitiveAnyIntersect (TriangleMesh tris) ray obj = intersectRayAnyTriangleList tris 0 ray obj
primitiveAnyIntersect (SparseOctreeModel svo') ray _ = SparseVoxelOctree.anyIntersect ray 0 50 svo' -- TODO Need to transform ray by inverse object matrix
primitiveAnyIntersect primitive' ray obj = primitiveClosestIntersect primitive' ray obj

-- -------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Family of bounding radius functions (post-xfrom by localToWorld matrix)
primitiveBoundingRadius :: Primitive -> Matrix -> Vector -> Double

primitiveBoundingRadius (Sphere sphereRadius) xform pos = sphereRadius + (pos `distance` getTranslation xform)
primitiveBoundingRadius (Plane _ _) _ _ = 0
primitiveBoundingRadius (TriangleMesh tris) _ centre = triangleListRadius 0 tris centre -- TODO - need to factor in world matrix
primitiveBoundingRadius (Box (boxMin, boxMax)) xform pos = (boxMin `distance` boxMax) + (pos `distance` getTranslation xform) -- TODO - need to factor in world matrix
primitiveBoundingRadius (SparseOctreeModel svo_) xform pos = boundingRadius svo_ + (pos `distance` getTranslation xform)

triangleListRadius :: Double -> [Triangle] -> Vector -> Double
triangleListRadius maximumRadius (tri:tris) centre = triangleListRadius (Prelude.max maximumRadius triangleRadius) tris centre
    where
      radii = map (\v -> centre `distance` vertPosition v) (vertices tri)
      triangleRadius = foldl' Prelude.max 0 radii
triangleListRadius maximumRadius [] _ = maximumRadius

-- -------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Family of bounding box functions

-- Find the bounding box of a primitive in world space
primitiveBoundingBox :: Primitive -> Object -> Maybe AABB
primitiveBoundingBox (Sphere sphereRadius) obj = Just (boxMin, boxMax)
    where
      boxMin = getCentre obj <-> Vector sphereRadius sphereRadius sphereRadius 0
      boxMax = getCentre obj <+> Vector sphereRadius sphereRadius sphereRadius 0
primitiveBoundingBox (Plane _ _) _ = Nothing
primitiveBoundingBox (TriangleMesh tris) obj = Just $ triangleListBoundingBox initialInvalidBox (transform obj) tris
primitiveBoundingBox (Box box) _ = Just box -- TODO Need to transform this by object's matrix
primitiveBoundingBox (SparseOctreeModel svo_) _ = Just $ boundingBox svo_ -- TODO need to transform by this object's matrix

-- Bounding box of a list of something
triangleListBoundingBox :: AABB -> Matrix -> [Triangle] -> AABB
triangleListBoundingBox currentBox transformMatrix (tri:tris) = triangleListBoundingBox (boundingBoxUnion currentBox thisTriangleBox) transformMatrix tris
    where
      worldSpaceVertices = map (transformVector transformMatrix . vertPosition) (vertices tri)
      (invalidMin, invalidMax) = initialInvalidBox
      thisTriangleBox = (foldl' Vector.min invalidMin worldSpaceVertices, foldl' Vector.max invalidMax worldSpaceVertices)
triangleListBoundingBox currentBox _ [] = currentBox

objectListBoundingBox :: [Object] -> AABB
objectListBoundingBox = foldr (boundingBoxUnion . (\obj -> fromMaybe initialInvalidBox (primitiveBoundingBox (primitive obj) obj))) initialInvalidBox

-- Does a primitive intersect a box?
-- Could maybe generalise the primitiveClosestIntersect function above via further pattern matching?
intersectsBox :: Primitive -> Matrix -> AABB -> Bool
intersectsBox (Sphere sphereRadius) matrix (boxMin, boxMax) = (centreX + sphereRadius) >= vecX boxMin && (centreX - sphereRadius) <= vecX boxMax &&
                                                              (centreY + sphereRadius) >= vecY boxMin && (centreY - sphereRadius) <= vecY boxMax &&
                                                              (centreZ + sphereRadius) >= vecZ boxMin && (centreZ - sphereRadius) <= vecZ boxMax
    where
      centre = getTranslation matrix
      centreX = vecX centre
      centreY = vecY centre
      centreZ = vecZ centre

intersectsBox (Plane (_, _, planeNormal) planeD) _ box = signum minDistance /= signum maxDistance
    where
      minX = boundingBoxMinComponent vecX planeNormal box
      minY = boundingBoxMinComponent vecY planeNormal box
      minZ = boundingBoxMinComponent vecZ planeNormal box
      maxX = boundingBoxMaxComponent vecX planeNormal box
      maxY = boundingBoxMaxComponent vecY planeNormal box
      maxZ = boundingBoxMaxComponent vecZ planeNormal box
      minAgainstPlane = Vector minX minY minZ 1
      maxAgainstPlane = Vector maxX maxY maxZ 1
      minDistance = (planeNormal `dot3` minAgainstPlane) + planeD
      maxDistance = (planeNormal `dot3` maxAgainstPlane) + planeD
      
intersectsBox (TriangleMesh tris) matrix box = boundingBoxOverlaps box triListBox
    where
      triListBox = triangleListBoundingBox initialInvalidBox matrix tris
      
intersectsBox (Box primBox) _ box = boundingBoxOverlaps box primBox

intersectsBox (SparseOctreeModel svo_) _ box = boundingBoxOverlaps box (boundingBox svo_)

-- -------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Aspect querying of objects
infinite :: Primitive -> Bool
infinite (Plane _ _) = True
infinite _ = False

-- -------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Specialised intersection code for bounding volume hierarchies
sphereIntersect :: Double -> Vector -> Ray -> Maybe Double
sphereIntersect rad centre (Ray rayOrg rayDir _ rayLen)
    | (centre `distanceSq` rayOrg) < (rad * rad) = Just 0 -- Inside the sphere!
    | discriminant < 0 = Nothing
    | discriminant == 0 = Just ((-b) / 2)
    | root1 >= 0 && root1 <= rayLen = Just root1
    | root2 >= 0 && root2 <= rayLen = Just root2
    | otherwise = Nothing
    where 
      delta = rayOrg <-> centre
      b = 2.0 * (delta `dot3` rayDir)
      c = (delta `dot3` delta) - rad**2
      discriminant = b**2 - 4 * c -- A is 1 because the ray direction is normalised
      root1 = (-b - sqrt discriminant) / 2
      root2 = (-b + sqrt discriminant) / 2
