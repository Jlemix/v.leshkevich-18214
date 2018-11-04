module Geometry
    where

data Shape = Circle Double | Square Double | Right_Triangle Double | Sphere Double | Cube Double | Cone Double Double

area::Shape->Double
area (Circle r) = pi*r*r
area (Square a) = a*a
area (Right_Triangle a) = (a*a*sqrt(3))/4

perimeter::Shape->Double
perimeter (Circle r) = 2*pi*r
perimeter (Square a) = a*4
perimeter (Right_Triangle a) = a*3

volume::Shape->Double
volume (Sphere r) = (4/3)*pi*r*r*r
volume (Cube a) = a*a*a
volume (Cone h r) = (h/3)*pi*r*r

surfaceArea::Shape->Double
surfaceArea (Sphere r) = 4*r*r*pi
surfaceArea (Cube a) = 6*a*a
surfaceArea (Cone r l) = (pi*r)*(r + l)
