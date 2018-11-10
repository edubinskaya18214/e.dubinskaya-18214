module Geometry(
	area,
	perimeter,
	surfaceArea,
	volume
)where

data Stereometric =  Scope Double | Cube Double | Tetrahedron Double | Cylinder Double Double | Cone Double Double deriving (Show, Eq)

surfaceArea::Stereometric -> Double
surfaceArea (Scope r) = 4 * pi * r * r
surfaceArea (Cube a) = a * a * a
surfaceArea (Tetrahedron a) = a * a * sqrt(3)
surfaceArea (Cylinder r h) = 2 * pi * r * h + 2 * pi * r * r
surfaceArea (Cone r h) = pi * r * r + pi * r * (l r h)
 where
   l a b = sqrt( r*r + h*h )

volume::Stereometric -> Double
volume (Scope r) = 4/3 * pi * r * r * r
volume (Cube a) = a * a * a
volume (Tetrahedron a) = a * a * a * sqrt (2) / 12
volume (Cylinder r h) = pi * r* r * h
volume (Cone r h) = 1 / 3 * pi * r * r * h

data Planimetric = Circle Double | ProperTriangle Double | Square Double | Rectangle Double Double deriving (Show, Eq)

area::Planimetric -> Double
area (Circle r) = pi * r * r
area (ProperTriangle a) = sqrt (3) * a * a / 4
area (Square a) = a * a
area (Rectangle a b) = a*b

perimeter:: Planimetric -> Double
perimeter (Circle r) = 2 * pi * r
perimeter (ProperTriangle a) = 3 * a
perimeter (Square a) = 4 * a
perimeter (Rectangle a b) = 2 * ( a + b )
