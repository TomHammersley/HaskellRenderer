-- Module for general primitives and intersections
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}

module Primitive (primitiveBoundingRadius, 
                  primitiveClosestIntersect, 
                  primitiveAnyIntersect,
                  primitiveTangentSpace, 
                  Object(Object), 
                  Primitive(Sphere, Plane, TriangleMesh, Box), 
                  primitive, 
                  material, 
                  makeTriangle, 
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
                  Vertex) where

import PolymorphicNum
import Ray
import Vector
import Material
import Matrix
import BoundingBox
import Data.Maybe
import Data.List

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
               | Box { size :: {-# UNPACK #-} !Vector } deriving (Show, Eq)

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
makeTriangle :: Position -> Position -> Position -> Triangle
makeTriangle v1 v2 v3 = Triangle verts newPlane newHalfPlanes
    where newPlane = makePlane v1 v2 v3
          newTanSpace = planeTangentSpace newPlane
          verts = map (\v -> Vertex v zeroVector newTanSpace) [v1, v2, v3]
          edgeVertices = [v1, v2, v3]
          edges = map normalise [v2 <-> v1, v3 <-> v2, v1 <-> v3]
          edgeNormals = map (\edge -> normalise $ tsNormal newTanSpace `cross` edge) edges
          -- TODO - The two vectors passed here are just dummies but they can fairly easily be derived
          newHalfPlanes = zipWith (\edgeNormal edgeVertex -> Plane (Vector 1 0 0 1, Vector 0 1 0 1, edgeNormal) (-(edgeNormal `dot3` edgeVertex))) edgeNormals edgeVertices

makeTriangleWithTangents :: Position -> Position -> Position -> Direction -> Direction -> Triangle
makeTriangleWithTangents v1 v2 v3 tangent binormal = Triangle verts newPlane newHalfPlanes
    where newPlane = makePlaneWithTangents v1 v2 v3 tangent binormal
          newTanSpace = planeTangentSpace newPlane
          verts = map (\v -> Vertex v zeroVector newTanSpace) [v1, v2, v3]
          edgeVertices = [v1, v2, v3]
          edges = map normalise [v2 <-> v1, v3 <-> v2, v1 <-> v3]
          edgeNormals = map (\edge -> normalise (tsNormal newTanSpace `cross` edge)) edges
          -- TODO - The two vectors passed here are just dummies but they can fairly easily be derived
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
intersectRayTriangle :: Ray -> Triangle -> Bool -> (# Bool, Double, Triangle #)
intersectRayTriangle ray triangle doubleSided
    | not doubleSided && direction ray `dot3` (tsNormal . planeTangentSpace . plane) triangle >= 0 = (# False, 0, triangle #)
    | otherwise = case planeIntersect (plane triangle) ray of
                    Nothing -> (# False, 0, triangle #)
                    Just (dist', _) -> if pointInsideTriangle triangle (pointAlongRay ray dist')
                                       then (# True, dist', triangle #)
                                       else (# False, 0, triangle #)

-- Intersect against a list of triangles
intersectRayTriangleList :: [Triangle] -> Int -> Maybe (Double, Int) -> Ray -> Object -> Maybe (Double, Int)
intersectRayTriangleList (x:xs) index acc ray obj = intersectRayTriangleList xs (index + 1) acc' ray' obj
    where
      (ray', acc') = case intersectRayTriangle ray x False of
                       (# False, _, _ #) -> (ray, acc)
                       (# True, dist, _ #) -> (shortenRay ray dist, Just (dist, index))
intersectRayTriangleList [] _ acc _ _ = acc

-- Intersect against any triangle
intersectRayAnyTriangleList :: [Triangle] -> Int  -> Ray -> Object -> Maybe (Double, Int)
intersectRayAnyTriangleList (x:xs) index ray obj = case intersectRayTriangle ray x False of
                                                     (# False, _, _ #) -> intersectRayAnyTriangleList xs (index + 1) ray obj
                                                     (# True, dist, _ #) -> Just (dist, index)
intersectRayAnyTriangleList [] _ _ _ = Nothing

-- Get the interpolated vertex normal
interpolatedTangentSpace :: Triangle -> Double -> Double -> Double -> TangentSpace
interpolatedTangentSpace triangle triAlpha triBeta triGamma = (tangent, binormal, normal)
    where [(tan1, bi1, norm1), (tan2, bi2, norm2), (tan3, bi3, norm3)] = map vertTangentSpace (vertices triangle)
          tangent = normalise $ tan1 <*> triAlpha <+> tan2 <*> triBeta <+> tan3 <*> triGamma
          binormal = normalise $ bi1 <*> triAlpha <+> bi2 <*> triBeta <+> bi3 <*> triGamma
          normal = normalise $ norm1 <*> triAlpha <+> norm2 <*> triBeta <+> norm3 <*> triGamma

planeIntersect :: Primitive -> Ray -> Maybe (Double, Int)
planeIntersect (Plane (_, _, planeNormal) planeD) (Ray rayOrg rayDir rayLen)
    | dirDotNormal == 0 = Nothing
    | intercept >= 0 && intercept <= rayLen = Just (intercept, 0)
    | otherwise = Nothing
    where dirDotNormal = rayDir `dot3` planeNormal
          intercept = ((-planeD) - (rayOrg `dot3` planeNormal)) / dirDotNormal
planeIntersect _ _ = undefined -- only available for this case

-- -------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Family of intersection functions
primitiveClosestIntersect :: Primitive -> Ray -> Object -> Maybe (Double, Int)

-- This function intersects a ray with a sphere, and returns the closest intercept
primitiveClosestIntersect (Sphere sphereRadius) (Ray rayOrg rayDir rayLen) obj
    | discriminant < 0 = Nothing
    | discriminant == 0 = Just (((-b) ** 0.5), 0)
    | root1 >= 0 && root1 <= rayLen = Just (root1, 0)
    | root2 >= 0 && root2 <= rayLen = Just (root2, 0)
    | otherwise = Nothing
    where 
      delta = rayOrg <-> getCentre obj
      b = 2 * (delta `dot3` rayDir)
      c = (delta `dot3` delta) - sphereRadius ** 2
      discriminant = b ** 2 - 4 * c -- A is 1 because the ray direction is normalised
      root1 = ((-b) - sqrt discriminant) * 0.5
      root2 = ((-b) + sqrt discriminant) * 0.5

-- This function intersects a ray with a plane and returns the closest intercept
primitiveClosestIntersect pln@(Plane (_, _, _) _) ray@(Ray _ _ _) _ = planeIntersect pln ray

-- Find intersection with a triangle mesh
primitiveClosestIntersect (TriangleMesh tris) ray obj = intersectRayTriangleList tris 0 Nothing ray obj

primitiveClosestIntersect (Box sz) (Ray rayOrg@(Vector ox oy oz _) rayDir@(Vector dx dy dz _) rayLen) _ -- TODO Need to transform ray by inverse object matrix
  | dx == 0 && (ox < vecX bounds0 || ox > vecX bounds1) = Nothing
  | dy == 0 && (oy < vecY bounds0 || oy > vecY bounds1) = Nothing
  | dz == 0 && (oz < vecZ bounds0 || oz > vecZ bounds1) = Nothing
  | otherwise = case tMinMax of
                     Nothing -> Nothing
                     Just (tmin, tmax) -> if tmin > tmax || 
                                             tmax < 0 || 
                                             tmin > rayLen then Nothing
                                          else Just (tmin, 0)
  where
    bounds0 = sz <*> (-0.5 :: Double)
    bounds1 = sz <*> (0.5 :: Double)
    intX = nearestSlabIntersection vecX
    intY = nearestSlabIntersection vecY
    intZ = nearestSlabIntersection vecZ
    tMinMax = maybePairFunctor Prelude.max Prelude.min intX (maybePairFunctor Prelude.max Prelude.min intY intZ)
    nearestSlabIntersection f
      | f rayDir == 0 = Nothing
      | t1 > t2 = Just (t2, t1)
      | otherwise = Just (t1, t2)
      where
        t1 = ((f bounds0) - (f rayOrg)) / (f rayDir)
        t2 = ((f bounds1) - (f rayOrg)) / (f rayDir)
                    
primitiveAnyIntersect :: Primitive -> Ray -> Object -> Maybe (Double, Int)
primitiveAnyIntersect (TriangleMesh tris) ray obj = intersectRayAnyTriangleList tris 0 ray obj
primitiveAnyIntersect primitive' ray obj = primitiveClosestIntersect primitive' ray obj

-- -------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Family of normal query functions
primitiveTangentSpace :: Primitive -> Int -> Position -> Object -> TangentSpace

-- Normal for a sphere
primitiveTangentSpace (Sphere sphereRadius) _ intersectionPoint obj = (tangent, binormal, normal)
    where
      tangent = Vector 1 0 0 0 -- This is clearly incorrect - fix this later!
      binormal = Vector 0 1 0 0
      normal = (intersectionPoint <-> getCentre obj) <*> (1 / sphereRadius)

primitiveTangentSpace (Plane planeNormal _) _ _ _ = planeNormal
primitiveTangentSpace (TriangleMesh tris) triId intersectionPoint _ = interpolatedTangentSpace triangle triAlpha triBeta triGamma
    where triangle = tris !! triId
          (triAlpha, triBeta, triGamma) = calculateBarycentricCoordinates intersectionPoint triangle
          
-- Unimplemented (no specialised implementation) case
primitiveTangentSpace _ _ _ _ = undefined

-- -------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Family of bounding radius functions
primitiveBoundingRadius :: Primitive -> Matrix -> Vector -> Double

primitiveBoundingRadius (Sphere sphereRadius) sphereTransform pos = sphereRadius + (pos `distance` getTranslation sphereTransform)
primitiveBoundingRadius (Plane _ _) _ _ = 0
primitiveBoundingRadius (TriangleMesh tris) _ centre = triangleListRadius 0 tris centre
primitiveBoundingRadius (Box sze) _ _ = Vector.magnitude sze

triangleListRadius :: Double -> [Triangle] -> Vector -> Double
triangleListRadius maximumRadius (tri:tris) centre = triangleListRadius (Prelude.max maximumRadius triangleRadius) tris centre
    where
      radii = map (\v -> centre `distance` (vertPosition v)) (vertices tri)
      triangleRadius = foldl' Prelude.max 0 radii
triangleListRadius maximumRadius [] _ = maximumRadius

-- -------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Family of bounding box functions

-- Find the bounding box of a primitive
primitiveBoundingBox :: Primitive -> Object -> Maybe AABB
primitiveBoundingBox (Sphere sphereRadius) obj = Just (boxMin, boxMax)
    where
      boxMin = getCentre obj <-> Vector sphereRadius sphereRadius sphereRadius 0
      boxMax = getCentre obj <+> Vector sphereRadius sphereRadius sphereRadius 0
primitiveBoundingBox (Plane _ _) _ = Nothing
primitiveBoundingBox (TriangleMesh tris) obj = Just $ triangleListBoundingBox initialInvalidBox (transform obj) tris
primitiveBoundingBox (Box sz) _ = Just (sz <*> (-0.5 :: Double), sz <*> (0.5 :: Double)) -- TODO Need to transform this by object's matrix

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
      minX = selectMinBoxComponent vecX planeNormal box
      minY = selectMinBoxComponent vecY planeNormal box
      minZ = selectMinBoxComponent vecZ planeNormal box
      maxX = selectMaxBoxComponent vecX planeNormal box
      maxY = selectMaxBoxComponent vecY planeNormal box
      maxZ = selectMaxBoxComponent vecZ planeNormal box
      minAgainstPlane = Vector minX minY minZ 1
      maxAgainstPlane = Vector maxX maxY maxZ 1
      minDistance = (planeNormal `dot3` minAgainstPlane) + planeD
      maxDistance = (planeNormal `dot3` maxAgainstPlane) + planeD
      
intersectsBox (TriangleMesh tris) matrix box = boundingBoxOverlaps box triListBox
    where
      triListBox = triangleListBoundingBox initialInvalidBox matrix tris
      
intersectsBox _ _ _ = undefined

-- -------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Aspect querying of objects
infinite :: Primitive -> Bool
infinite (Plane _ _) = True
infinite _ = False

-- -------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Specialised intersection code for bounding volume hierarchies
sphereIntersect :: Double -> Vector -> Ray -> Maybe Double
sphereIntersect rad centre (Ray rayOrg rayDir rayLen)
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
