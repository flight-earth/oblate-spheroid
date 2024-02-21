import Std.Lean.Float
import Earth
import Geodesy.Problems
import Units

namespace Geodesy.Vincenty

open Float
open Geodesy.Problems
open Earth.Ellipsoid
open Units.Convert

structure GeodeticAccuracy where
  geodeticAccuracy: Float

def sinSq := sin ∘ sin

def cos2 (sigma1 sigma: Float) : Float × Float :=
  let x := (2.0 * sigma1 + sigma).cos
  (x, x * x)

def auxLat (f : Float): Float -> Float :=
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

-- The solution to the direct geodesy problem with input latitude unchecked and
-- longitude not normalized.
--
-- Symbol reference from Vincenty's paper.
-- a, b   = major and minor semiaxes of the ellipsoid
-- f      = flattening (a - b) / a
-- Φ      = geodetic latitude, positive north of the equator
-- L      = difference in longitude, positive east
-- s      = length of the geodesic
-- α₁, α₂ = azimuths o the geodesic, clockwise rom the north: α₂ in the direction P₁ P₂ produced
-- α      = azimuth of the geodesic at the equator
-- U      = reduced latitude, defined by tan U = (1 - f) tan Φ1
-- λ      = difference in longitude on an auxillary sphere
-- σ      = angular distance P₁ P₂, on the sphere
-- σ1     = angular distance on the sphere from the equator to P₁
-- σm     = angular distance on the sphere rom the equator to the midpoint of the line
def directUnchecked
    (ellipsoid : Ellipsoid)
    (accuracy : GeodeticAccuracy)
    (p : DirectProblem) : DirectSolution :=
  let ⟨x, ⟨az1⟩, ⟨s⟩⟩  := p
  let lat1 := x.lat.rad
  let lng1 := x.lat.rad

  let ⟨a⟩ := ellipsoid.equatorialR
  let ⟨b⟩ := polarRadius ellipsoid
  let f := flattening ellipsoid

  -- Initial setup
  let xU1 := (auxLat f) lat1
  let cosU1 := xU1.cos
  let sinU1 := xU1.sin

  -- NOTE: In some transcriptions of Vincenty's formula to code the following
  -- are sometimes seen for calculating U1, cosU1 and sinU1.
  -- val tanU1 = (1. - f) * tan (float Φ1)
  -- val cosU1 = 1. / sqrt (1. + tanU1 * tanU1)
  -- val sinU1 = tanU1 * cosU1
  --
  -- SEE: https:--www.purplemath.com/modules/idents.htm
  -- secx = 1 / cosx
  -- tan²x + 1 = sec²x
  -- tanx = sinx / cosx

  let cosAz1 := az1.cos
  let sinAz1 := az1.sin
  let sigma1 := atan2 xU1.tan cosAz1

  let sinAlpha := cosU1 * sinAz1
  let sinSqAlpha := sinAlpha * sinAlpha
  let cosSqAlpha := 1.0 - sinSqAlpha

  let uSq :=
      let aSq := a * a
      let bSq := b * b
      cosSqAlpha * (aSq - bSq) / bSq

  let xA := 1.0 + uSq / 16384.0 * (4096.0 + uSq * (-768.0 + uSq * (320.0 - 175.0 * uSq)))
  let xB := uSq / 1024.0 * (256.0 + uSq * (-128.0 + uSq * (74.0 - 47.0 * uSq)))

  -- Solution
  let sigma := iterateAngularDistance accuracy xA xB s b sigma1 (s / (b * xA))

  let sinSigma := sigma.sin
  let cosSigma := sigma.cos

  let v := sinU1 * cosSigma + cosU1 * sinSigma * cosAz1

  let (j, j') :=
      let sinU1SinSigma := sinU1 * sinSigma
      let cosU1CosSigmaCosAz1 := cosU1 * cosSigma * cosAz1
      (   sinU1SinSigma  - cosU1CosSigmaCosAz1
      , -(sinU1SinSigma) + cosU1CosSigmaCosAz1
      )

  let w := (1.0 - f) * sqrt (sinSqAlpha + j * j)
  let lat2 := atan2 v w
  let lambda := atan2 (sinSigma * sinAz1) (cosU1 * cosSigma - sinU1 * sinSigma * cosAz1)
  let xC := f / 16.0 * cosSqAlpha * (4.0 + f * (4.0 - 3.0 * cosSqAlpha))

  let diffLng :=
      let (cos2x, cos2xSq) := cos2 sigma1 sigma
      let y' := cos2x + xC * cosSigma * (-1.0 + 2.0 * cos2xSq)
      let x' := sigma + xC * sinSigma * y'
      lambda - (1.0 - xC) * f * sinAlpha * x'

  let lng2 := diffLng + lng1

  { y := ⟨⟨lat2⟩, ⟨lng2⟩⟩
  , az2 := some ∘ Az.mk $ atan2 sinAlpha j'
  }

-- The solution to the direct geodesy problem with input latitude rejected
-- outside the range -90° .. 90° and longitude normalized to -180° .. 180°.
def direct (e : Ellipsoid) (a : GeodeticAccuracy) (p: DirectProblem) : Except String DirectSolution :=
  let ⟨x, ⟨az1⟩, _⟩ := p
  let xLat := x.lat.rad
  match (xLat |> isPlusMinusHalfPiRad) with
  | none =>
      throw $ "Latitude of " ++ (radToDeg xLat).toString ++ " is outside -90° .. 90° range"

  | some nLat =>
      let nLng := plusMinusPiRad x.lng.rad
      let xNorm := {lat := Units.DMS.Rad.mk nLat, lng := Units.DMS.Rad.mk nLng}
      let azNorm := normalizeRad az1

      pure $ directUnchecked e a {p with x := xNorm, az1 := Az.mk azNorm}
