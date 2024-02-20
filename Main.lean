import LatLng
import Earth
import Geodesy
import Units

open LatLng
open Geodesy.Problems
open Geodesy.Haversines
open Units.DMS
open Units.Convert

def london : LatLng :=
  { lat := Rad.mk $ degToRad (Deg.mk 51.5007).deg
  , lng := Rad.mk $ degToRad (Deg.mk (-0.1246)).deg
  }

def newyork : LatLng :=
  { lat := Rad.mk $ degToRad (Deg.mk 40.6892).deg
  , lng := Rad.mk $ degToRad (Deg.mk (-74.0445)).deg
  }

def distanceLonNyc := distance london newyork
def inverseLonNyc := inverse ⟨london, newyork⟩

/-- info: 5574840.456848554313182830810546875 -/
#guard_msgs in #eval distanceLonNyc
/--
info: (s=5574840.456848554313182830810546875, az1=-1.250757751225103131531568578793667256832122802734375, az2=(some 1))
-/
#guard_msgs in #eval inverseLonNyc

def main : IO Unit := do
  let dms : DMS := ⟨90, 12, 0.999⟩

  IO.println s!"London: {london}"
  IO.println s!"New York: {newyork}"
  IO.println s!"Distance LON -> NYC: {distanceLonNyc}"
  IO.println s!"DMS: {dms}"

  testShow
  testEq
  testFromDeg
  testToDeg
  testNormalize
