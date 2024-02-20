import Std
import Std.Lean.Float
import Std.Data.String.Basic
import Init.Data
import Units.Convert

namespace Units.DMS

open String
open Ordering

-- SEE: https://leanprover.zulipchat.com/#narrow/stream/113489-new-members/topic/How.20to.20use.20Std.2EHashMap/near/408935525
open Units.Convert renaming Deg → UDeg, Rad → URad
open Units.Convert (radToDeg)

structure Deg where deg : UDeg deriving BEq
structure Rad where rad: URad deriving BEq

def fromRad (r : Rad) : Deg := Deg.mk <| radToDeg r.rad

structure DMS where
  deg : Int
  min : Int
  sec : Float
  deriving BEq

def signum (x : Float) : Ordering := if x < 0.0 then lt else if x > 0.0 then gt else eq

instance : ToString Ordering where
  toString
    | lt => "lt"
    | eq => "eq"
    | gt => "gt"

-- | When positive or zero, all fields of a DMS are positive. When negative, the
-- first nonzero field in order of decreasing size should be negative but no
-- other fields but if smaller fields are negative, ignore their sign.
--
-- As a structure, a DMS can be constructed with more than one negative field so
-- signDMS serves separate the negativity of the fields from the fields.
def signDMS : DMS -> Ordering × DMS
  | ⟨d, m, s⟩ =>
    let sign := if d < 0 || m < 0 || s < 0 then lt else if d == 0 && m == 0 && s == 0 then eq else gt
    (sign, ⟨if d < 0 then -d else d, if m < 0 then -m else m, s.abs⟩)

#guard signDMS (DMS.mk 0 0 (-1.0)) == (lt, ⟨0, 0, 1.0⟩)
#guard signDMS (DMS.mk 0 (-1) 0) == (lt, ⟨0, 1, 0.0⟩)
#guard signDMS (DMS.mk (-1) 0 0) == (lt, ⟨1, 0, 0.0⟩)
#guard signDMS (DMS.mk 0 0 0) == (eq, ⟨0, 0, 0.0⟩)
#guard signDMS (DMS.mk 0 0 1) == (gt, ⟨0, 0, 1.0⟩)
#guard signDMS (DMS.mk 0 1 0) == (gt, ⟨0, 1, 0.0⟩)
#guard signDMS (DMS.mk 1 0 0) == (gt, ⟨1, 0, 0.0⟩)

def div (n : Float) (d : Int) : Int :=
  let n' := Float.abs n
  let d' := Float.ofInt d
  let d'' := Float.abs d'
  (n' / d'').floor.toUInt64.toNat
  |> fun x => if signum n == signum d'' then x else -x

def mod (n : Float) (d : Int) : Float :=
  n - Float.ofInt (div n d * d)

def divMod (n : Float) (d : Int) : (Int × Float) :=
  let q := div n d
  (q, n - Float.ofInt (q * d))

def fromDeg (α : Deg) : DMS :=
  let dAbs := α.deg.abs
  let dFrac := dAbs - dAbs.floor
  let (mm, mFrac) := divMod (dFrac * 60.0) 1

  let dd := dAbs.floor.toUInt64.toNat
  DMS.mk (match signum α.deg with | eq => 0 | lt => Int.neg dd | gt => dd) mm (mFrac * 60.0)

def toDeg (dms : DMS) : Deg :=
  let (pn, dms') := signDMS dms
  let d := Float.ofInt dms'.deg
  let m := Float.ofInt dms'.min
  let s := dms'.sec
  let dd := (d * 3600.0 + m * 60.0 + s) / 3600.0
  Deg.mk $ match pn with | eq => 0.0 | gt => dd | lt => -dd

def normalizeDeg (d : Deg) : Deg :=
  let x := mod d.deg.abs 360
  if d.deg < 0.0 then Deg.mk (360.0 - x) else Deg.mk x

def normalizeDMS (dms : DMS) : DMS := fromDeg ∘ normalizeDeg $ toDeg dms

def displayDMS (x : DMS) : String :=
  toString x.deg ++ "°" ++
  toString x.min ++ "′" ++
  x.sec.toStringFull ++ "″"

instance : ToString Deg where
  toString := fun x => x.deg.toStringFull ++ "°"

instance : ToString DMS where
  toString := displayDMS

def checkShow (res : String) (tst : Unit -> DMS)  (name := res):=
  let got := toString $ tst ()
  let msg := if (got == res) then s!"ok: {res}" else s!"failed!:\n expect: {res}\n gotten: {got}"
  IO.println s!"{name}: {msg}"

def checkEq (res : DMS) (tst : Unit -> DMS) (name := toString res):=
  let got := tst ()
  let msg := if (got == res) then s!"ok: {res}" else "failed!:\n expect: {res}\n gotten: {got}"
  IO.println s!"eq {name}: {msg}"

def checkNormalize (res : DMS) (tst : Unit -> DMS) (name := "normalize" ++ toString res) :=
  checkEq res tst name

def checkFromDeg (res : DMS) (tst : Unit -> DMS) (name := toString res):=
  let got := tst ()
  let msg := if (got == res) then s!"ok: {res}" else "failed!:\n expect: {res}\n gotten: {got}"
  IO.println s!"from-deg {name}: {msg}"

def checkToDeg (res : Deg) (tst : Unit -> Deg) (name := toString res):=
  let got := tst ()
  let msg := if (got == res) then s!"ok: {res}" else "failed!:\n expect: {res}\n gotten: {got}"
  IO.println s!"from-deg {name}: {msg}"

def testShow : IO Unit := do
  let v := "90°12′0.5″"
  checkShow v (fun _ => ⟨90, 12, 0.5⟩)

def testEq : IO Unit := do
  let v := DMS.mk 90 12 0.9999
  checkEq v (fun _ => ⟨90, 12, 0.9999⟩)

def testFromDeg : IO Unit := do
  let dmsZero := DMS.mk 0 0 0.0
  let dmsOne := DMS.mk 1 0 0.0
  let dmsMinusOne := DMS.mk (-1) 0 0.0
  let dms169 := DMS.mk 169 3 59.99999839625161
  let dmsMinus169 := DMS.mk (-169) 3 59.99999839625161
  checkFromDeg (fromDeg $ Deg.mk 0.0) (fun _ => dmsZero)
  checkFromDeg (fromDeg $ Deg.mk 1.0) (fun _ => dmsOne)
  checkFromDeg (fromDeg $ Deg.mk (-1.0)) (fun _ => dmsMinusOne)
  checkFromDeg (fromDeg $ Deg.mk 169.06666666622118) (fun _ => dms169)
  checkFromDeg (fromDeg $ Deg.mk (-169.06666666622118)) (fun _ => dmsMinus169)

def testToDeg : IO Unit := do
  checkToDeg (toDeg ⟨0, 0, 0.0⟩ ) (fun _ => ⟨0.0⟩)
  checkToDeg (toDeg ⟨289, 30, 0.0⟩) (fun _ => ⟨289.5⟩)

def testNormalize : IO Unit := do
  let f : Float -> DMS := fun x => Deg.mk x |> fromDeg |> normalizeDMS
  checkNormalize (DMS.mk   0  0  0.0) (fun _ => f 0.0)
  checkNormalize (DMS.mk 180  0  0.0) (fun _ => f 180.0)
  checkNormalize (DMS.mk   1  0  0.0) (fun _ => f 1.0)
  checkNormalize (DMS.mk 180  0  0.0) (fun _ => f (-180.0))
  checkNormalize (DMS.mk 359  0  0.0) (fun _ => f (-1.0))

  checkNormalize (DMS.mk 0  0  0.0) (fun _ => (DMS.mk 0  0  0.0) |> normalizeDMS)
  checkNormalize (DMS.mk 180 0 0.0) (fun _ => (DMS.mk 180  0  0.0) |> normalizeDMS)
  checkNormalize (DMS.mk 180 0 0.0) (fun _ => (DMS.mk (-180) 0  0.0) |> normalizeDMS)
  checkNormalize (DMS.mk 359 0 0.0) (fun _ => (DMS.mk (-1) 0 0.0) |> normalizeDMS)

  checkNormalize (DMS.mk 190 56 1.6037483874242753e-6)
    (fun _ => (DMS.mk 190 56 1.6037483874242753e-6) |> normalizeDMS)

  checkNormalize (DMS.mk 169 3 59.99999839625161)
    (fun _ => (DMS.mk (-190) 56 1.603721102583222e-6) |> normalizeDMS)

/-- info: -169°3′59.99999839625161257572472095489501953125″ -/
#guard_msgs in #eval fromDeg $ Deg.mk (-169.06666666622118)

/-- info: 1°0′0″ -/
#guard_msgs in #eval ⟨1, 0, 0.0⟩ |> normalizeDMS
/-- info: 0°1′0″ -/
#guard_msgs in #eval ⟨0, 1, 0.0⟩ |> normalizeDMS
/-- info: 0°0′1″ -/
#guard_msgs in #eval ⟨0, 0, 1.0⟩ |> normalizeDMS

/-- info: 1° -/
#guard_msgs in #eval normalizeDeg $ Deg.mk (361.0)
/-- info: 1°0′0″ -/
#guard_msgs in #eval ⟨361, 0, 0.0⟩ |> normalizeDMS
/-- info: 1°0′0″ -/
#guard_msgs in #eval normalizeDMS ∘ fromDeg $ Deg.mk (361.0)

/-- info: 359° -/
#guard_msgs in #eval normalizeDeg $ Deg.mk (-1.0)
/-- info: 359°0′0″ -/
#guard_msgs in #eval ⟨-1, 0, 0.0⟩ |> normalizeDMS
/-- info: 359°0′0″ -/
#guard_msgs in #eval normalizeDMS ∘ fromDeg $ Deg.mk (-1.0)

/-- info: 359°59′0.00000000005456968210637569427490234375″ -/
#guard_msgs in #eval fromDeg ∘ normalizeDeg $ Deg.mk (-1.0/60.0)
/-- info: 359°59′0.00000000005456968210637569427490234375″ -/
#guard_msgs in #eval ⟨0, -1, 0.0⟩ |> normalizeDMS

/-- info: 359°59′58.999999999932697392068803310394287109375″ -/
#guard_msgs in #eval fromDeg ∘ normalizeDeg $ Deg.mk (-1.0/3600.0)
/-- info: 359°59′58.999999999932697392068803310394287109375″ -/
#guard_msgs in #eval ⟨0, 0, -1.0⟩ |> normalizeDMS
/-- info: 359°59′58.999999999932697392068803310394287109375″ -/
#guard_msgs in #eval fromDeg ∘ normalizeDeg $ Deg.mk (360.0 - 1.0/3600.0)
/-- info: 359°59′58.999999999932697392068803310394287109375″ -/
#guard_msgs in #eval ⟨0, 0, -1.0⟩ |> toDeg |> fromDeg ∘ normalizeDeg

/-- info: -0.0002777777777777777775368439616698879035538993775844573974609375° -/
#guard_msgs in #eval Deg.mk (-1.0/3600.0)
/-- info: -0.0002777777777777777775368439616698879035538993775844573974609375° -/
#guard_msgs in #eval ⟨0, 0, -1.0⟩ |> toDeg

/-- info: 359.9997222222222035270533524453639984130859375° -/
#guard_msgs in #eval normalizeDeg $ Deg.mk (-1.0/3600.0)
/-- info: 359.9997222222222035270533524453639984130859375° -/
#guard_msgs in #eval normalizeDeg $ Deg.mk (360.0 - 1.0/3600.0)

/-- info: "60" -/
#guard_msgs in #eval ⟨0, 0, 60.0⟩ |> toDeg |> (fun d => (d.deg * 3600.0).toStringFull)
/-- info: "61.00000000000000710542735760100185871124267578125" -/
#guard_msgs in #eval ⟨0, 0, 61.0⟩ |> toDeg |> (fun d => (d.deg * 3600.0).toStringFull)
/-- info: "61.00000000000000710542735760100185871124267578125" -/
#guard_msgs in #eval ⟨0, 1, 1.0⟩ |> toDeg |> (fun d => (d.deg * 3600.0).toStringFull)
/-- info: "1" -/
#guard_msgs in #eval ⟨0, 0, 1.0⟩ |> toDeg |> (fun d => (d.deg * 3600.0).toStringFull)
/-- info: 0°1′0″ -/
#guard_msgs in #eval ⟨0, 0, 60.0⟩ |> normalizeDMS
/-- info: 1°0′0″ -/
#guard_msgs in #eval ⟨0, 60, 0.0⟩ |> normalizeDMS
