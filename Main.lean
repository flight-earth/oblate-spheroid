import LatLng
import Earth
import Geodesy
import Units

open LatLng
open Geodesy.Problems
open Units.DMS
open Units.Convert

def main : IO Unit := do
  let london : LatLng :=
    { lat := Rad.mk $ degToRad (Deg.mk 51.5007).deg
    , lng := Rad.mk $ degToRad (Deg.mk (-0.1246)).deg
    }

  let newyork : LatLng :=
    { lat := Rad.mk $ degToRad (Deg.mk 40.6892).deg
    , lng := Rad.mk $ degToRad (Deg.mk (-74.0445)).deg
    }

  let dms : DMS := ⟨90, 12, 0.999⟩

  IO.println s!"London: {london}"
  IO.println s!"New York: {newyork}"
  IO.println s!"DMS: {dms}"

  testShow
  testEq
  testFromDeg
  testToDeg
  testNormalize
