import Std.Lean.Float

namespace Geodesy.Vincenty

open Float

structure GeodeticAccuracy where
  geodeticAccuracy: Float

def sinSq := sin ∘ sin

def cos2 (sigma1 sigma: Float) : Float × Float :=
  let x := (2.0 * sigma1 + sigma).cos
  (x, x * x)

def auxLat (f: Float): Float -> Float :=
  atan ∘ (fun x => (1.0 - f) * x) ∘ tan

partial def iterateAngularDistance
  (accuracy : GeodeticAccuracy) xA xB s (b: Float) sigma1 sigma :=
  let tolerance := accuracy.geodeticAccuracy
  let (cos2x, cos2xsq) := cos2 sigma1 sigma
  let sinSigma := sin sigma
  let cosSigma := sigma.cos
  let sinSqSigma := sinSigma * sinSigma

  let deltaSigma :=
          xB * sinSigma *
              (cos2x + xB / 4.0 *
                  (cosSigma * (-1.0 + 2.0 * cos2xsq)
                      - xB / 6.0
                          * cos2x
                          * (-3.0 + 4.0 * sinSqSigma)
                          * (-3.0 + 4.0 * cos2xsq)))

  let sigma' := s / (b * xA) + deltaSigma
  if (sigma - sigma').abs < tolerance
      then sigma
      else iterateAngularDistance accuracy xA xB s b sigma1 sigma'
