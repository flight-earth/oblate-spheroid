import Std.Lean.Float

import Earth
import Geodesy.Problems
import LatLng
import Units

namespace Geodesy.PointToPoint.Vincenty

open Float

open Earth.Ellipsoid
open Geodesy.Problems
open Geodesy.Problems.AbnormalLatLng
open Geodesy.Problems.GeodeticInverse
open Geodesy.PointToPoint.Vincenty
open LatLng
open Units.Angle
open Units.Convert
open Units.Convert renaming Deg → UDeg, Rad → URad
open Units.DMS

-- | In Vincenty's paper he says,
--
-- "The inverse formula may give no solution over a line between
-- two nearly antipodal points. This will occur when λ, as computed
-- by eqn. (11), is greater than π in absolute value."
--
-- (45°,-179°59'58.17367'') to (45°,180°)
-- Comparing the above two points, longitudes are less than a minute apart.  To
-- be able to get the difference using simple subtraction normalize the
-- longitudes to a range 0 <= lng <= 2π.
def normalizeLng (lng : Float) : Float :=
    mod' lng (2 * pi)

/-! Undefine lambda
-- SEE: https://leanprover.zulipchat.com/#narrow/stream/270676-lean4/topic/Undefine.20lambda/near/393695422
-/
--macro "λ" : term => pure (Lean.mkIdent `«λ»)

def tooFar : Dist := ⟨2000000.0⟩

def minLatBound := toRad ∘ fromDeg $ Deg.mk (-90.0)
def maxLatBound := toRad ∘ fromDeg $ Deg.mk 90.0
def minLngBound := toRad ∘ fromDeg $ Deg.mk (-180.0)
def maxLngBound := toRad ∘ fromDeg $ Deg.mk 180.0

structure InverseStep where
    tolerance : GeodeticAccuracy
    a : Float
    b : Float
    f : Float
    L : Float
    sinU₁ : Float
    sinU₂ : Float
    cosU₁ : Float
    cosU₂ : Float
    sinU₁sinU₂ : Float
    cosU₁cosU₂ : Float

partial def iloop (step : InverseStep) («λ» : Float) : GeodeticInverse :=
    if abs «λ» > pi then GeodeticInverseAntipodal else
        let ⟨tolerance, a, b, f, L, sinU₁, sinU₂, cosU₁, cosU₂, sinU₁sinU₂, cosU₁cosU₂⟩ := step

        let «sinλ» := sin «λ»
        let «cosλ» := cos «λ»
        -- WARNING: The sign of numerator and denominator are important
        -- in atan2. The sign of each term below follows Vincenty's
        -- 1975 paper "Direct and Inverse Solutions of Geodesics on the
        -- Ellipsoid with Application of Nested Equations"
        --
        -- i' = cosU₁ * sinλ
        -- j' = -sinU₁ * cosU₂ + cosU₁ * sinU₂ * cosλ
        --
        -- By contrast Delorme's 1978 paper "Evaluation Direct and
        -- Inverse Geodetic Algorithms" has this formulation with the
        -- signs reversed.
        --
        -- i' = -cosU₁ * sinλ
        -- j' = sinU₁ * cosU₂ - cosU₁ * sinU₂ * cosλ
        --
        -- As the method is Vincenty's I'm going their signage.
        let i' := cosU₁ * «sinλ»
        let j' := -sinU₁ * cosU₂ + cosU₁ * sinU₂ * «cosλ»

        let i := cosU₂ * «sinλ»
        let j := cosU₁ * sinU₂ - sinU₁ * cosU₂ * «cosλ»

        let «sin²σ» := i * i + j * j
        let sinσ := sqrt «sin²σ»
        let cosσ := sinU₁sinU₂ + cosU₁cosU₂ * «cosλ»

        let σ := atan2 sinσ cosσ

        let sinα := cosU₁cosU₂ * «sinλ» / sinσ
        let «cos²α» := 1 - sinα * sinα
        let C := f / 16 * «cos²α» * (4 + f * (4 - 3 * «cos²α»))
        let «u²» := let «b²» := b * b; «cos²α» * (a * a - «b²») / «b²»

        -- NOTE: Start and end points on the equator, _C = 0.
        let cos2σm := if «cos²α» == 0 then 0 else cosσ - 2 * sinU₁sinU₂ / «cos²α»
        let «cos²2σm» := cos2σm * cos2σm

        let A := 1 + «u²» / 16384 * (4096 + «u²» * (-768 + «u²» * (320 - 175 * «u²»)))
        let B := «u²» / 1024 * (256 + «u²» * (-128 + «u²» * (74 - 47 * «u²»)))

        let y :=
            cosσ * (-1 + 2 * «cos²2σm»)
            - B / 6 * cos2σm * (-3 + 4 * «sin²σ») * (-3 + 4 * «cos²2σm»)

        let Δσ := B * sinσ * (cos2σm + B / 4 * y)

        let x := cos2σm + C * cosσ * (-1 + 2 * «cos²2σm»)
        let «λ'» := L + (1 - C) * f * sinα * (σ + C * sinσ * x)

        let s := b * A * (σ - Δσ)
        let α₁ := atan2 i j
        let α₂ := atan2 i' j'

        if abs («λ» - «λ'») >= tolerance.accuracy
            then iloop step «λ'»
            else GeodeticInverse $ InverseSolution.mk ⟨s⟩ ⟨α₁⟩ (some ⟨α₂⟩)

partial def inverse
    (ellipsoid : Ellipsoid)
    (tolerance : GeodeticAccuracy)
    (p : InverseProblem) : GeodeticInverse :=
    let ⟨Φ₁, L₁⟩ := p.x
    let ⟨Φ₂, L₂⟩ := p.y
    let a : Float := ellipsoid.equatorialR.radius
    let b : Float := (polarRadius ellipsoid).radius
    let f := flattening ellipsoid
    let auxLat : Float -> Float := atan ∘ ((1 - f) * ·) ∘ tan
    let U₁ := auxLat Φ₁.rad
    let U₂ := auxLat Φ₂.rad
    let L :=
        let L' := L₂.rad - L₁.rad
        if abs L' <= pi
          then L'
          else normalizeLng L₂.rad - normalizeLng L₁.rad
    let sinU₁ := sin U₁
    let sinU₂ := sin U₂
    let cosU₁ := cos U₁
    let cosU₂ := cos U₂
    let sinU₁sinU₂ := sinU₁ * sinU₂
    let cosU₁cosU₂ := cosU₁ * cosU₂

    let step : InverseStep := ⟨tolerance, a, b, f, L, sinU₁, sinU₂, cosU₁, cosU₂, sinU₁sinU₂, cosU₁cosU₂⟩
    iloop step L


def distanceUnchecked (ellipsoid : Ellipsoid) (prob : InverseProblem) : GeodeticInverse :=
    if prob.x == prob.y
      then GeodeticInverse.GeodeticInverse $
        InverseSolution.mk ⟨0.0⟩ ⟨0.0⟩ (some (Az.mk (toRad ∘ fromDeg $ Deg.mk 180).rad))
      -- REVIEW: Why is LT comparison not decidable?
      --     failed to synthesize instance
      -- Decidable (prob.x.lat < minLatBound)
      else if prob.x.lat.rad < minLatBound.rad
      then GeodeticInverseAbnormal LatUnder
      else if prob.x.lat.rad > maxLatBound.rad
      then GeodeticInverseAbnormal LatOver
      else if prob.x.lng.rad < minLngBound.rad
      then GeodeticInverseAbnormal LngUnder
      else if prob.x.lng.rad > maxLngBound.rad
      then GeodeticInverseAbnormal LngOver
      else if prob.y.lat.rad < minLatBound.rad
      then GeodeticInverseAbnormal LatUnder
      else if prob.y.lat.rad > maxLatBound.rad
      then GeodeticInverseAbnormal LatOver
      else if prob.y.lng.rad < minLatBound.rad
      then GeodeticInverseAbnormal LngUnder
      else if prob.y.lng.rad > maxLngBound.rad
      then GeodeticInverseAbnormal LngOver
      else inverse ellipsoid defaultGeodeticAccuracy prob

def distance (e : Ellipsoid) (x y : LatLng) : Except String Dist :=
    let qx := x.lat
    let qy := y.lat
    let msg :=
        let sx := toString (normalizeDMS ∘ fromDeg $ fromRad qx)
        let sy := toString (normalizeDMS ∘ fromDeg $ fromRad qy)
        s!"Latitude of {sx} or {sy} is outside -90° .. 90° range"

    let prob : Option InverseProblem := do
        let xLat : DMS := fromDeg $ fromRad x.lat
        let yLat : DMS := fromDeg $ fromRad y.lat
        let xLng : DMS := fromDeg $ fromRad x.lng
        let yLng : DMS := fromDeg $ fromRad y.lng

        let xLat' : DMS <- Angle.plusMinusHalfPi xLat
        let yLat' : DMS <- Angle.plusMinusHalfPi yLat

        let xLng' := Angle.plusMinusPi xLng
        let yLng' := Angle.plusMinusPi yLng

        let x' := LatLng.mk (toRad xLat') (toRad xLng')
        let y' := LatLng.mk (toRad yLat') (toRad yLng')
        pure $ InverseProblem.mk x' y'

    match prob with
    | none => Except.error msg
    | some prob => Except.ok $
        match (distanceUnchecked e prob) with
        | GeodeticInverseAntipodal => tooFar
        | GeodeticInverseAbnormal _ => tooFar
        | GeodeticInverse.GeodeticInverse i => i.s
