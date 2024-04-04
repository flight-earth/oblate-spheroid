import Std
import Std.Data.String.Basic
import Earth.Ellipsoid
import Geodesy.Haversines
import Geodesy.Problems
import Geodesy.Cylinder.Vincenty
import Geodesy.PointToPoint.Vincenty
import LatLng
import Units.Convert
import Units.DMS

open Earth.Ellipsoid
open Geodesy.Haversines (distance)
open Geodesy.PointToPoint.Vincenty renaming distance → vincentyDistance
open Geodesy.Problems
open LatLng
open Units
open Units.Convert renaming Deg → UDeg, Rad → URad
open Units.DMS
-- open Units.DMS (DMS Rad toRad fromRad fromDeg normalizeDMS absDiffDMS absDiffDMS180)

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
abbrev TestTolerance := Dist
def tolerance : Dist := ⟨0.008⟩

-- From the paper, Vincenty's errors were mm of -0.4, -0.4, -0.7, -0.2 and -0.8.
def indirectDistanceTolerances : List TestTolerance :=
    Dist.mk <$>
    [ 0.000404
    , 0.000387
    , 0.000703
    , 0.000197
    , 0.000787
    ]

def azTolerance : DMS := ⟨0, 0, 0.016667⟩

abbrev DiffDMS := DMS → DMS → DMS
abbrev AzTolerance := DMS
abbrev SpanLatLng := LatLng → LatLng → Dist
abbrev AzimuthFwd := LatLng → LatLng → Option Rad
abbrev AzimuthBwd := LatLng → LatLng → Option Rad

def describeInverseDistance (x y : LatLng) (sExpected : Dist) (tolerance : TestTolerance) :=
    toString x
    ++ " to "
    ++ toString y
    ++ " = "
    ++ toString sExpected
    ++ " ± "
    ++ toString tolerance

def describeAzimuthFwd (x y : LatLng) (azActual : Option Rad) (azExpected : Az) (tolerance : AzTolerance) :=
    toString x
    ++ " to "
    ++ toString y
    ++ " -> "
    ++ toString (fromDeg $ fromRad ⟨azExpected.az⟩)
    ++ " ± "
    ++ toString tolerance
    ++ " ("
    ++ (toString $ (normalizeDMS ∘ fromDeg ∘ fromRad) <$> azActual)
    ++ ")"

def describeAzimuthRev (x y : LatLng) (azActual azExpected : Option Rad) (tolerance : AzTolerance) :=
    toString x
    ++ " to "
    ++ toString y
    ++ " <- "
    ++ (toString $ (normalizeDMS ∘ fromDeg ∘ fromRad) <$> azExpected)
    ++ " ± "
    ++ toString tolerance
    ++ " ("
    ++ (toString $ (normalizeDMS ∘ fromDeg ∘ fromRad) <$> azActual)
    ++ ")"

abbrev Assertion := IO Unit

def assertFailure (msg : String) : IO Unit := IO.println msg
def unlessb (b : Bool) (m : IO Unit) : IO Unit := if b then pure () else m

def assertCompare [ToString a] (preface : String) (compare : a → a → Bool) (cmpSymbol : String) (key : a) (actual : a) : Assertion :=
    unlessb
        (compare actual key)
        (let msg := (if "" == preface then "" else preface ++ "\n") ++ "expected: " ++ toString actual ++ cmpSymbol ++ toString key
        assertFailure msg)

def assertCompareWith [ToString a] (convert : a → b) (preface : String) (compare : b → b → Bool) (cmpSymbol : String) (key : a) (actual : a) : Assertion :=
    unlessb
        (compare (convert actual) (convert key))
        (let msg := (if "" == preface then "" else preface ++ "\n") ++ "expected: " ++ toString actual ++ cmpSymbol ++ toString key
        assertFailure msg)

infixr:50 " @?<= " => assertCompare "" (· <= ·) " <= "

set_option quotPrecheck false
infixr:50 " @?<=Deg " => assertCompareWith (fun x => (toDeg x).deg) "" (· <= ·) " <= "

def testCase (name : String) (test : Assertion) : Assertion :=
    IO.println name *> test

def diff (x y : Float) : Float := Float.abs (x - y)
def azToDms (az : Az) : DMS := fromDeg $ fromRad ⟨az.az⟩
def radToDms : Rad → DMS := fromDeg ∘ fromRad

instance : LT DMS where
  lt x y := toDeg x < toDeg y

structure CmpDMS where
  cmp : DMS
  deriving BEq

instance : LT CmpDMS where
  lt x y := toDeg x.cmp < toDeg y.cmp

def inverseChecks
    (diffAzFwd : DiffDMS)
    (diffAzRev : DiffDMS)
    (distTolerances : List TestTolerance)
    (azTolerance : AzTolerance)
    (spans : List SpanLatLng)
    (azFwds : List AzimuthFwd)
    (azRevs : List AzimuthBwd)
    (solns : List InverseSolution)
    (probs : List InverseProblem) : List Assertion :=
        let f := fun
            (distTolerance : TestTolerance)
            (span : SpanLatLng)
            (azFwd : AzimuthFwd)
            (azRev : AzimuthBwd)
            (soln : InverseSolution)
            (prob : InverseProblem) =>
                let ⟨⟨s⟩ , α₁, α₂⟩ := soln
                let ⟨x, y⟩ := prob
                let s' := span x y
                let α₁' := azFwd x y
                let α₂' := azRev x y

                [ do
                    let actual : Float := diff s'.dist s
                    testCase (describeInverseDistance x y ⟨s⟩ tolerance)
                    $ actual @?<= distTolerance.dist

                , do
                    match ((flip diffAzFwd) (azToDms α₁) <$> (radToDms <$> α₁')) with
                    | none => pure ()
                    | some actual =>
                        let expected : DMS := azTolerance
                        testCase (describeAzimuthFwd x y α₁' α₁ azTolerance)
                        -- TODO: Find out why, "failed to synthesize instance LE DMS"
                        $ actual @?<=Deg expected

                , do
                    match ((diffAzRev ∘ radToDms) <$> α₂' <*> (azToDms <$> α₂)), α₂ with
                    | none, _ => pure ()
                    | _, none => pure ()
                    | some actual, some rev =>
                        let expected : DMS := azTolerance
                        testCase (describeAzimuthRev x y α₂' (some ⟨rev.az⟩) azTolerance)
                        -- TODO: Find out why, "failed to synthesize instance LE DMS"
                        $ actual @?<=Deg expected
                ]

        List.join $
        List.zipWith
            (fun (t, span) ((azFwd, azRev), (soln, prob)) => f t span azFwd azRev soln prob)
            (List.zip distTolerances spans)
            (List.zip
                (List.zip azFwds azRevs)
                (List.zip solns probs))

def vincentyUnits : IO Unit := do
    let diffAzFwd : DiffDMS := absDiffDMS
    let diffAzRev : DiffDMS := absDiffDMS180

    let spanLatLng : SpanLatLng := (fun x y =>
        match vincentyDistance bessel x y with
        | Except.error _ => ⟨0⟩
        | Except.ok d => d)

    -- TODO: Add directChecks to checks
    let checks := inverseChecks
        diffAzFwd
        diffAzRev
        indirectDistanceTolerances
        azTolerance
        (List.replicate 5 spanLatLng)
        (List.replicate 5 (fun _ _ => none))
        (List.replicate 5 (fun _ _ => none))
        inverseSolutions
        inverseProblems

    for check in checks do
        check
