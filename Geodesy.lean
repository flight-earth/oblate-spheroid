namespace Geodesy

structure LatLng where
  lat : Float
  lng : Float

instance : ToString LatLng where
  toString x := toString (x.lat, x.lng)

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
  az2 : Az

structure InverseSolution where
  s : Dist
  az1 : Az
  az2 : Az
