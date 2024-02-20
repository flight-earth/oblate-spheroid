import Units
import LatLng

namespace Geodesy.Problems

open Units
open LatLng

structure Az where
  az : Float

structure Dist where
  dist : Float

structure DirectProblem where
  x : LatLng
  az1 : Az
  s : Dist

structure InverseProblem where
  x : LatLng
  y : LatLng

structure DirectSolution where
  y : LatLng
  az2 : Option Az

structure InverseSolution where
  s : Dist
  az1 : Az
  az2 : Option Az
