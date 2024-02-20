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

instance : ToString Dist where
  toString d := d.dist.toStringFull

instance : ToString Az where
  toString a := a.az.toStringFull

instance : ToString DirectProblem where
  toString p := s!"(x={p.x}, az1={p.az1}, s={p.s})"

instance : ToString InverseProblem where
  toString p := s!"(x={p.x}, y={p.y})"

instance : ToString DirectSolution where
  toString s := s!"(y={s.y}, az2={s.az2})"

instance : ToString InverseSolution where
  toString s := s!"(s={s.s}, az1={s.az1}, az2={s.az2})"
