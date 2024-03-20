import Earth.Ellipsoid
import Geodesy.Haversines
import Geodesy.Problems
import LatLng
import Units.Convert
import Units.DMS

open Earth.Ellipsoid
open Geodesy.Haversines (distance)
open Geodesy.Problems
open LatLng
open Units
open Units.Convert renaming Deg → UDeg, Rad → URad
open Units.DMS (DMS Rad toRad)

namespace Geodesy.Published.Vincenty1975

def ellipsoids : List Ellipsoid :=
  [ bessel, hayford, hayford, hayford, hayford ]

-- Distances in meters
def distances : List Float :=
    [     14110526.170
    ,      4085966.703
    ,      8084823.839
    ,     19960000.000
    ,     19780006.558
    ]

def xAzimuths : List DMS :=
    [ ⟨ 96, 36,  8.79960⟩
    , ⟨ 95, 27, 59.63089⟩
    , ⟨ 15, 44, 23.74850⟩
    , ⟨ 89,  0,  0.0⟩
    , ⟨  4, 59, 59.99995⟩
    ]

def yAzimuths : List DMS :=
    [ ⟨ 137, 52, 22.01454⟩
    , ⟨ 118,  5, 58.96161⟩
    , ⟨ 144, 55, 39.92147⟩
    , ⟨  91,  0,  6.11733⟩
    , ⟨ 174, 59, 59.88481⟩
    ]

def inverseProblemData : List ((DMS × DMS) × (DMS × DMS)) :=
    [ (⟨⟨ 55, 45,  0.00000⟩, ⟨  0,  0,  0.0⟩⟩, ⟨⟨-33, 26,  0.00000⟩, ⟨108, 13,  0.00000⟩⟩)
    , (⟨⟨ 37, 19, 54.95367⟩, ⟨  0,  0,  0.0⟩⟩, ⟨⟨ 26,  7, 42.83946⟩, ⟨ 41, 28, 35.50729⟩⟩)
    , (⟨⟨ 35, 16, 11.24862⟩, ⟨  0,  0,  0.0⟩⟩, ⟨⟨ 67, 22, 14.77638⟩, ⟨137, 47, 28.31435⟩⟩)
    , (⟨⟨  1,  0,  0.00000⟩, ⟨  0,  0,  0.0⟩⟩, ⟨⟨ 0, -59, 53.83076⟩, ⟨179, 17, 48.02997⟩⟩)
    , (⟨⟨  1,  0,  0.00000⟩, ⟨  0,  0,  0.0⟩⟩, ⟨⟨  1,  1, 15.18952⟩, ⟨179, 46, 17.84244⟩⟩)
    ]

def toInverseProblem (x y : (DMS × DMS)) : InverseProblem := InverseProblem.mk
    (let ⟨xLat, xLng⟩ := x; (LatLng.mk (toRad xLat) (toRad xLng)))
    (let ⟨yLat, yLng⟩ := y; (LatLng.mk (toRad yLat) (toRad yLng)))

def inverseProblems : List InverseProblem :=
    inverseProblemData |> List.map (fun (x, y) => toInverseProblem x y)

def inverseSolutions : List InverseSolution :=
    List.zipWith
        (fun d (x, y) => InverseSolution.mk ⟨d⟩ ⟨(toRad x).rad⟩ (some ⟨(toRad y).rad⟩))
        distances
        (List.zip xAzimuths yAzimuths)
