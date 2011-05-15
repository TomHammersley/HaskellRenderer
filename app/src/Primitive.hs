-- Module for general primitives and intersections
{-# LANGUAGE BangPatterns #-}

module Primitive (primitiveBoundingRadius, 
                  primitiveClosestIntersect, 
                  primitiveAnyIntersect,
                  primitiveTangentSpace, 
                  Object(Object), 
                  Primitive(Sphere, Plane, TriangleMesh), 
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
                  infinitePrimitive, 
                  boundingBoxValid, 
                  sphereIntersect,
                  TangentSpace) where

import Ray
import Vector
import Material
import Matrix
import BoundingBox
import Misc
import Data.Maybe

-- Triangle object used for triangle meshes
data Vertex = Vertex { vertPosition :: {-# UNPACK #-} !Position, 
                       vertUV :: {-# UNPACK #-} !Position, 
                       vertTangentSpace :: {-# UNPACK #-} !TangentSpace } deriving (Show, Read, Eq)
data Triangle = Triangle { vertices :: [Vertex], plane :: Primitive, halfPlanes :: [Primitive] } deriving (Show, Read, Eq)

-- General object definition
data Object = Object { primitive :: Primitive,
                       material :: Material,
                       transform :: !Matrix} deriving (Show, Read, Eq)

-- Different kinds of primitives that an object can have
data Primitive = Sphere { radius :: !Float }
               | Plane { planeTangentSpace :: {-# UNPACK #-} !TangentSpace, planeDistance :: {-# UNPACK #-} !Float }
               | TriangleMesh { triangles :: [Triangle] } deriving (Show, Read, Eq)

-- Get the centre of an object
getCentre :: Object -> Vector
getCentre !object = getTranslation $ transform object

-- Surface normal for 3 points
surfaceNormal :: Position -> Position -> Position -> Direction
surfaceNormal !v1 !v2 !v3 = (v2 - v1) `cross` (v3 - v1)

-- Make a plane
makePlane :: Position -> Position -> Position -> Primitive
makePlane !v1 !v2 !v3 = Plane (tangent, binormal, normal) (-(v1 `dot3` normal))
    where 
      !normal = normalise (surfaceNormal v1 v2 v3)
      !tangent = normalise $ v2 - v1
      !binormal = normalise $ v3 - v1

-- -------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Triangle base functionality

-- Make a triangle
makeTriangle :: Position -> Position -> Position -> Triangle
makeTriangle !v1 !v2 !v3 = Triangle verts newPlane newHalfPlanes
    where newPlane = makePlane v1 v2 v3
          newTanSpace = planeTangentSpace newPlane
          verts = map (\v -> Vertex v zeroVector newTanSpace) [v1, v2, v3]
          edgeVertices = [v1, v2, v3]
          edges = map normalise [v2 - v1, v3 - v2, v1 - v3]
          edgeNormals = map (\edge -> normalise $ thr newTanSpace `cross` edge) edges
          -- TODO - The two vectors passed here are just dummies but they can fairly easily be derived
          newHalfPlanes = zipWith (\edgeNormal edgeVertex -> Plane (Vector 1 0 0 1, Vector 0 1 0 1, edgeNormal) (-(edgeNormal `dot3` edgeVertex))) edgeNormals edgeVertices

makeQuad :: [Position] -> [Triangle]
makeQuad [vert1, vert2, vert3, vert4] = [makeTriangle vert1 vert2 vert3, makeTriangle vert1 vert3 vert4]
makeQuad _ = error "makeQuad: List was invalid size"

-- Turn a list of quad vertices into a triangle list
quadsToTriangles :: [Position] -> [Triangle]
quadsToTriangles positions = quadsToTriangles' positions []

quadsToTriangles' :: [Position] -> [Triangle] -> [Triangle]
quadsToTriangles' verts currentList 
    | length verts >= 4 = quadsToTriangles' (drop 4 verts) (currentList ++ makeQuad (take 4 verts))
    | otherwise = currentList

-- Area of a triangle
triangleArea :: Position -> Position -> Position -> Float
triangleArea !v1 !v2 !v3 = 0.5 * magnitude (surfaceNormal v1 v2 v3)

-- Calculate the barycentric co-ordinates of a point on a triangle
calculateBarycentricCoordinates :: Position -> Triangle -> (Float, Float, Float)
calculateBarycentricCoordinates !pos !triangle = (alpha, beta, gamma)
    where !alpha = triangleArea pos v2 v3 / area
          !beta = triangleArea pos v1 v3 / area
          !gamma = 1 - alpha - beta
          !area = triangleArea v1 v2 v3
          [!v1, !v2, !v3] = map vertPosition (vertices triangle)

-- Distance to a plane
distanceToPlane :: Primitive -> Vector -> Float
distanceToPlane (Plane !(_, _, norm) !dist) !pos = pos `dot3` norm + dist
distanceToPlane _ _ = error "distanceToPlane: Unsupport primitive for this function"

-- Use halfplanes to test if a point is inside a triangle
pointInsideTriangle :: Triangle -> Position -> Bool
pointInsideTriangle !tri !point = all (\pln -> distanceToPlane pln point >= 0) (halfPlanes tri)

-- Intersect a ray with a triangle
intersectRayTriangle :: Ray -> Object -> Triangle -> Bool -> Maybe (Float, Triangle)
intersectRayTriangle !ray !obj !triangle !doubleSided
    | not doubleSided && direction ray `dot3` (thr . planeTangentSpace . plane) triangle > 0 = Nothing
    | otherwise = case primitiveClosestIntersect (plane triangle) ray obj of
                    Nothing -> Nothing
                    Just (!dist', _) -> if pointInsideTriangle triangle (pointAlongRay ray dist')
                                        then Just (dist', triangle)
                                        else Nothing

-- Intersect against a list of triangles
intersectRayTriangleList :: [Triangle] -> Int -> Maybe (Float, Int) -> Ray -> Object -> Maybe (Float, Int)
intersectRayTriangleList !(x:xs) !index !currentResult !currentRay !obj = intersectRayTriangleList xs (index + 1) newResult newRay obj
    where
      (!newRay, !newResult) = case intersectRayTriangle currentRay obj x False of
                                Nothing -> (currentRay, currentResult)
                                Just (dist, _) -> (shortenRay currentRay dist, Just (dist, index))
intersectRayTriangleList [] _ !currentResult _ _ = currentResult

-- Intersect against any triangle
intersectRayAnyTriangleList :: [Triangle] -> Int  -> Ray -> Object -> Maybe (Float, Int)
intersectRayAnyTriangleList !(x:xs) !index !ray !obj = case intersectRayTriangle ray obj x True of
                                                         Nothing -> intersectRayAnyTriangleList xs (index + 1) ray obj
                                                         Just (!dist, _) -> Just (dist, index)
intersectRayAnyTriangleList [] _ _ _ = Nothing

-- Get the interpolated vertex normal
interpolatedTangentSpace :: Triangle -> Float -> Float -> Float -> TangentSpace
interpolatedTangentSpace !triangle !triAlpha !triBeta !triGamma = (tangent, binormal, normal)
    where [!(tan1, bi1, norm1), !(tan2, bi2, norm2), !(tan3, bi3, norm3)] = map vertTangentSpace (vertices triangle)
          tangent = normalise $ (tan1 <*> triAlpha) + (tan2 <*> triBeta) + (tan3 <*> triGamma)
          binormal = normalise $ (bi1 <*> triAlpha) + (bi2 <*> triBeta) + (bi3 <*> triGamma)
          normal = normalise $ (norm1 <*> triAlpha) + (norm2 <*> triBeta) + (norm3 <*> triGamma)

-- -------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Family of intersection functions
primitiveClosestIntersect :: Primitive -> Ray -> Object -> Maybe (Float, Int)

-- This function intersects a ray with a sphere, and returns the closest intercept
primitiveClosestIntersect (Sphere !sphereRadius) (Ray !rayOrg !rayDir !rayLen) obj
    | discriminant < 0 = Nothing
    | discriminant == 0 = Just ((-b) / 2, 0)
    | root1 >= 0 && root1 <= rayLen = Just (root1, 0)
    | root2 >= 0 && root2 <= rayLen = Just (root2, 0)
    | otherwise = Nothing
    where 
      !delta = rayOrg - getCentre obj
      !b = 2.0 * (delta `dot3` rayDir)
      !c = (delta `dot3` delta) - sphereRadius**2
      !discriminant = b**2 - 4 * c -- A is 1 because the ray direction is normalised
      !root1 = (-b - sqrt discriminant) / 2
      !root2 = (-b + sqrt discriminant) / 2

-- This function intersects a ray with a plane and returns the closest intercept
primitiveClosestIntersect (Plane !(_, _, planeNormal) !planeD) (Ray !rayOrg !rayDir !rayLen) _
    | dirDotNormal == 0 = Nothing
    | intercept >= 0 && intercept <= rayLen = Just (intercept, 0)
    | otherwise = Nothing
    where !dirDotNormal = rayDir `dot3` planeNormal
          !intercept = ((-planeD) - (rayOrg `dot3` planeNormal)) / dirDotNormal

-- Find intersection with a triangle mesh
primitiveClosestIntersect (TriangleMesh !tris) !ray !obj = intersectRayTriangleList tris 0 Nothing ray obj

primitiveAnyIntersect :: Primitive -> Ray -> Object -> Maybe (Float, Int)
primitiveAnyIntersect (TriangleMesh !tris) !ray !obj = intersectRayAnyTriangleList tris 0 ray obj
primitiveAnyIntersect primitive' ray obj = primitiveClosestIntersect primitive' ray obj

-- -------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Family of normal query functions
primitiveTangentSpace :: Primitive -> Int -> Position -> Object -> TangentSpace

-- Normal for a sphere
primitiveTangentSpace (Sphere !sphereRadius) _ !intersectionPoint obj = (tangent, binormal, normal)
    where
      tangent = Vector 1 0 0 1 -- This is clearly incorrect - fix this later!
      binormal = Vector 0 1 0 1
      normal = (intersectionPoint - getCentre obj) <*> (1 / sphereRadius)

primitiveTangentSpace (Plane !planeNormal _) _ _ _ = planeNormal
primitiveTangentSpace (TriangleMesh !tris) !triId !intersectionPoint _ = interpolatedTangentSpace triangle triAlpha triBeta triGamma
    where !triangle = tris !! triId
          (!triAlpha, !triBeta, !triGamma) = calculateBarycentricCoordinates intersectionPoint triangle

-- -------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Family of bounding radius functions
primitiveBoundingRadius :: Primitive -> Matrix -> Vector -> Float

primitiveBoundingRadius (Sphere sphereRadius) sphereTransform pos = sphereRadius + (pos `distance` getTranslation sphereTransform)
primitiveBoundingRadius (Plane _ _) _ _ = 0
primitiveBoundingRadius (TriangleMesh tris) _ centre = triangleListRadius 0 tris centre

triangleListRadius :: Float -> [Triangle] -> Vector -> Float
triangleListRadius maximumRadius (tri:tris) centre = triangleListRadius (Prelude.max maximumRadius triangleRadius) tris centre
    where
      radii = map (distance centre . vertPosition) (vertices tri)
      triangleRadius = foldr Prelude.max 0 radii 
triangleListRadius maximumRadius [] _ = maximumRadius

-- -------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Family of bounding box functions

-- Find the bounding box of a primitive
primitiveBoundingBox :: Primitive -> Object -> Maybe AABB
primitiveBoundingBox (Sphere sphereRadius) obj = Just (boxMin, boxMax)
    where
      boxMin = getCentre obj - Vector sphereRadius sphereRadius sphereRadius 0
      boxMax = getCentre obj + Vector sphereRadius sphereRadius sphereRadius 0
primitiveBoundingBox (Plane _ _) _ = Nothing
primitiveBoundingBox (TriangleMesh tris) obj = Just $ triangleListBoundingBox initialInvalidBox (transform obj) tris

-- Bounding box of a list of something
triangleListBoundingBox :: AABB -> Matrix -> [Triangle] -> AABB
triangleListBoundingBox currentBox transformMatrix (tri:tris) = triangleListBoundingBox (boundingBoxUnion currentBox thisTriangleBox) transformMatrix tris
    where
      worldSpaceVertices = map (transformVector transformMatrix . vertPosition) (vertices tri)
      (invalidMin, invalidMax) = initialInvalidBox
      thisTriangleBox = (foldr Vector.min invalidMin worldSpaceVertices, foldr Vector.max invalidMax worldSpaceVertices)
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
      minAgainstPlane = Vector (selectMinBoxComponent vecX planeNormal box) (selectMinBoxComponent vecY planeNormal box) (selectMinBoxComponent vecZ planeNormal box) 1
      maxAgainstPlane = Vector (selectMaxBoxComponent vecX planeNormal box) (selectMaxBoxComponent vecY planeNormal box) (selectMaxBoxComponent vecZ planeNormal box) 1
      minDistance = (planeNormal `dot3` minAgainstPlane) + planeD
      maxDistance = (planeNormal `dot3` maxAgainstPlane) + planeD
      
intersectsBox (TriangleMesh tris) matrix box = boundingBoxOverlaps box triListBox
    where
      triListBox = triangleListBoundingBox initialInvalidBox matrix tris

-- -------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Aspect querying of objects
infinitePrimitive :: Primitive -> Bool
infinitePrimitive (Plane _ _) = True
infinitePrimitive (TriangleMesh _) = False
infinitePrimitive _ = False

-- -------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Specialised intersection code for bounding volume hierarchies
sphereIntersect :: Float -> Vector -> Ray -> Maybe Float
sphereIntersect !rad !centre (Ray !rayOrg !rayDir !rayLen)
    | (centre `distanceSq` rayOrg) < (rad * rad) = Just 0 -- Inside the sphere!
    | discriminant < 0 = Nothing
    | discriminant == 0 = Just ((-b) / 2)
    | root1 >= 0 && root1 <= rayLen = Just root1
    | root2 >= 0 && root2 <= rayLen = Just root2
    | otherwise = Nothing
    where 
      !delta = rayOrg - centre
      !b = 2.0 * (delta `dot3` rayDir)
      !c = (delta `dot3` delta) - rad**2
      !discriminant = b**2 - 4 * c -- A is 1 because the ray direction is normalised
      !root1 = (-b - sqrt discriminant) / 2
      !root2 = (-b + sqrt discriminant) / 2
