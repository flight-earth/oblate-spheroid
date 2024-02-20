import Geodesy
import Units

open Geodesy.Problems
open Units.DMS
open Units.Convert

def main : IO Unit := do
  let london : LatLng := ⟨51.5007, -0.1246⟩
  let newyork : LatLng := ⟨40.6892, -74.0445⟩
  let dms : DMS := ⟨90, 12, 0.999⟩

  IO.println s!"London: {london}"
  IO.println s!"New York: {newyork}"
  IO.println s!"DMS: {dms}"

  testShow
  testEq
  testFromDeg
  testToDeg
  testNormalize
