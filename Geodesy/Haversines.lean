import Std
import Std.Lean.Float
import Earth
import LatLng
import Units
import Geodesy.Problems

namespace Geodesy.Haversines

open Units.Convert
open Units.Radius
open Units.DMS
open Earth.Sphere
open LatLng
open Float
open Geodesy.Problems

def haversine (x : Units.Convert.Rad) : Units.Convert.Rad := let y := sin (x / 2.0); y * y

def aOfHaversine (x y : LatLng) : Units.Convert.Rad :=
  let ⟨⟨xLat⟩, ⟨xLng⟩⟩ := x
  let ⟨⟨yLat⟩, ⟨yLng⟩⟩ := y
  let (dLat , dLng) := (yLat - xLat, yLng - xLng)
  let hLatF := haversine dLat
  let hLngF := haversine dLng

  hLatF + cos xLat * cos yLat * hLngF

def distance (x : LatLng) (y : LatLng) : Dist :=
  let ⟨rEarth⟩ := earthRadius
  let radDist := 2.0 * (aOfHaversine x y).sqrt.asin
  Dist.mk (radDist * rEarth)

def azimuthFwd' (x y : LatLng) : Units.Convert.Rad :=
  let ⟨⟨xLat⟩, ⟨xLng⟩⟩ := x
  let ⟨⟨yLat⟩, ⟨yLng⟩⟩ := y
  let deltaLng := yLng - xLng
  let x := sin deltaLng * cos yLat
  let y := cos xLat * sin yLat - sin xLat * cos yLat * cos deltaLng

  atan2 x y

def azimuthFwd (x y : LatLng) : Option Units.Convert.Rad := some $ azimuthFwd' x y

-- TODO: Implement rotate.
def rotate (_ _ : Units.DMS.Rad) : Units.DMS.Rad := ⟨1.0⟩

def azimuthRev (x y : LatLng) : Option Units.Convert.Rad :=
  (azimuthFwd y x)
  |> Option.map (fun az =>
      let ⟨az'⟩ := rotate (Rad.mk pi) (Rad.mk az)
      az')

def direct (prob : DirectProblem) : DirectSolution :=
  let ⟨⟨lat1⟩, ⟨lng1⟩⟩ := prob.x
  let az1 := prob.az1.az
  let ⟨earthR⟩ := earthRadius
  let d: Float := prob.s.dist
  let dR := d / earthR

  -- SEE: https://www.movable-type.co.uk/scripts/latlong.html
  let lat2 := asin $ sin lat1 * cos dR + cos lat1 * sin dR * cos az1
  let lng2 := lng1 + atan2 (sin az1 * sin dR * cos lat1) (cos dR - sin lat1 * sin lat2)
  let y : LatLng := {lat := ⟨lat2⟩, lng := ⟨lng2⟩}

  let az2 :=
      (azimuthFwd y ⟨⟨lat1⟩, ⟨lng1⟩⟩)
      |> Option.map (fun (az: Units.Convert.Rad) =>
          (Rad.mk az)
          |> rotate (Rad.mk pi)
          |> fromRad
          |> fromDeg
          |> normalizeDMS
          |> toDeg
          |> (fun x => x.deg)
          |> degToRad
          |> (fun x => Az.mk x))

  {y := {lat := ⟨lat2⟩, lng := ⟨lng2⟩}, az2 := az2}

def inverse (p : InverseProblem) : InverseSolution :=
  let ⟨x, y⟩ := p
  let x' : LatLng := {lat := x.lat, lng := x.lng}
  let y' : LatLng := {lat := y.lat, lng := y.lng}
  let az1 := Az.mk (azimuthFwd' x' y')
  let az2 := Option.map Az.mk (azimuthRev x' y')
  {s := distance x y, az1 := az1, az2 := az2}
