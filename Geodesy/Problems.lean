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

structure GeodeticAccuracy where
    accuracy : Float

def defaultGeodeticAccuracy := GeodeticAccuracy.mk $ 1 / 1000000000000

inductive AbnormalLatLng where
| LatUnder
| LatOver
| LngUnder
| LngOver

-- GeodeticInverse needs to be inhabited to be used in a partial recursive definition.
-- SEE: https://leanprover.zulipchat.com/#narrow/stream/270676-lean4/topic/.E2.9C.94.20partial.20recursive.20def.3A.20failed.20to.20show.20inhabited.20non.20empty/near/291308090
inductive GeodeticInverse where
| GeodeticInverseAbnormal : AbnormalLatLng -> GeodeticInverse
| GeodeticInverseAntipodal : GeodeticInverse
| GeodeticInverse : InverseSolution -> GeodeticInverse
deriving Inhabited
