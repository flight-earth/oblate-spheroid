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
open Units.DMS (DMS Rad toRad normalizeDMS)

-- Test data from ...
--
-- Direct and Inverse Solutions of Geodesics on the Ellipsoid with Applications
-- of Nested Equations
-- Survey Review XXII, 176
-- T. Vincenty, April 1975.
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

def directPairs : List (InverseProblem × InverseSolution) :=
    List.zip inverseProblems inverseSolutions

def directProblems := directPairs |> List.map (fun (p, _) => p)
def directSolutions := directPairs |> List.map (fun (_, s) => s)

-- Units of mm.
def tolerance : Float := 0.008

-- From the paper, Vincenty's errors were mm of -0.4, -0.4, -0.7, -0.2 and -0.8.
def indirectDistanceTolerances : List Float :=
    [ 0.000404
    , 0.000387
    , 0.000703
    , 0.000197
    , 0.000787
    ]

def azTolerance : DMS := ⟨0, 0, 0.016667⟩

-- Units of kilometers for distance and tolerance.
abbrev Distance := Float
abbrev TestTolerance := Float

abbrev DiffDMS := DMS → DMS → DMS
abbrev AzTolerance := DMS
abbrev SpanLatLng := LatLng → LatLng → Distance
abbrev AzimuthFwd := LatLng → LatLng → Option Rad
abbrev AzimuthBwd := LatLng → LatLng → Option Rad

def describeInverseDistance (x y : DMS) (sExpected : Distance) (tolerance : TestTolerance) :=
    toString x
    ++ " to "
    ++ toString y
    ++ " = "
    ++ toString sExpected
    ++ " ± "
    ++ toString tolerance

def describeAzimuthFwd (x y : DMS) (azActual : Option DMS) (azExpected : DMS) (tolerance : AzTolerance) :=
    toString x
    ++ " to "
    ++ toString y
    ++ " -> "
    ++ toString azExpected
    ++ " ± "
    ++ toString tolerance
    ++ " ("
    ++ (toString $ normalizeDMS <$> azActual)
    ++ ")"

def describeAzimuthRev (x y : DMS) (azActual : Option DMS) (azExpected : DMS) (tolerance : AzTolerance) :=
    toString x
    ++ " to "
    ++ toString y
    ++ " <- "
    ++ toString azExpected
    ++ " ± "
    ++ toString tolerance
    ++ " ("
    ++ (toString $ normalizeDMS <$> azActual)
    ++ ")"
