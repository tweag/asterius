
module Rotate (rotate) where
import Numbers
import Vectors
import Matrices

rotate	:: Vector -> Vector -> Vector
rotate v w	| v==0			= w
