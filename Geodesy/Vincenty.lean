import Std.Lean.Float

namespace Geodesy.Vincenty

open Float

def sinSq := sin ∘ sin

def auxLat (f: Float): Float -> Float :=
  atan ∘ (fun x => (1.0 - f) * x) ∘ tan
