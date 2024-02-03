import Std
import Std.Data.String.Basic
import Init.Data
open String
open Ordering

structure Deg where
  deg : Float
  deriving BEq

structure DMS where
  deg : Int
  min : Int
  sec: Float
  deriving BEq

def signum (x : Float) : Ordering := if x < 0.0 then lt else if x > 0.0 then gt else eq

instance : ToString Ordering where
  toString
    | lt => "lt"
    | eq => "eq"
    | gt => "gt"

def signDMS : DMS -> Ordering
  | ⟨d, m, s⟩ =>
    if d < 0 || m < 0 || s < 0 then lt else
      if d == 0 && m == 0 && s == 0 then eq else gt

/-- info: lt -/
#guard_msgs in #eval signDMS $ DMS.mk 0 0 (-1.0)
/-- info: lt -/
#guard_msgs in #eval signDMS $ DMS.mk 0 (-1) 0
/-- info: lt -/
#guard_msgs in #eval signDMS $ DMS.mk (-1) 0 0
/-- info: eq -/
#guard_msgs in #eval signDMS $ DMS.mk 0 0 0
/-- info: gt -/
#guard_msgs in #eval signDMS $ DMS.mk 0 0 1
/-- info: gt -/
#guard_msgs in #eval signDMS $ DMS.mk 0 1 0
/-- info: gt -/
#guard_msgs in #eval signDMS $ DMS.mk 1 0 0

def div (n : Float) (d : Int) : Int :=
  let n' := Float.abs n
  let d' := Float.ofInt d
  let d'' := Float.abs d'
  (n' / d'').floor.toUInt64.toNat
  |> fun x => if signum n == signum d'' then x else -x

def mod (n : Float) (d : Int) : Float := n - Float.ofInt (div n d * d)

def divMod (n : Float) (d : Int) : (Int × Float) :=
  let q := div n d
  (q, n - Float.ofInt (q * d))

def fromDeg (α : Deg) : DMS :=
  let dAbs := α.deg.abs
  let dFrac := dAbs - dAbs.floor
  let dd := (dAbs - dFrac).toUInt64.toNat
  let (mm, mFrac) := divMod (dFrac * 60.0) 1

  ⟨if signum α.deg == lt then Int.neg dd else dd, mm, mFrac * 60.0⟩

def toDeg (dms : DMS) : Deg :=
  let d := Float.abs $ Float.ofInt dms.deg
  let m := Float.abs $ Float.ofInt dms.min
  let s := dms.sec
  let m' := m + s / 60.0
  let d' := d + m' / 60.0
  if signDMS dms == lt then Deg.mk (-d') else Deg.mk d'

def dropTrailingZeros (s : String) : String :=
  match Matcher.find? (Matcher.ofString ".") s with
  | none => s
  | some dps =>
    {dps with stopPos := String.endPos s}.dropRightWhile (· == '0')
    |> fun s' => s.take s'.startPos.byteIdx ++ toString s'

def normalizeDeg (d : Deg) : Deg :=
  let x := mod d.deg 360
  if x == 0.0 then Deg.mk 0.0 else
    if x < 0.0 then Deg.mk (360.0 + x) else Deg.mk x

def normalizeDMS (dms : DMS) : DMS := fromDeg ∘ normalizeDeg $ toDeg dms

def displayDMS (x : DMS) : String :=
  toString x.deg ++ "°" ++
  toString x.min ++ "′" ++
  (dropTrailingZeros $ toString x.sec) ++ "″"

instance : ToString Deg where
  toString := fun x => toString x.deg ++ "°"

instance : ToString DMS where
  toString := displayDMS

#eval normalizeDMS (DMS.mk 0 0 0.0)
#eval normalizeDMS (DMS.mk 359 0 0.0)
#eval normalizeDMS (DMS.mk 361 0 0.0)
#eval normalizeDMS (DMS.mk (-1) 0 0.0)

/-- info: -169°3′59.999998″ -/
#guard_msgs in #eval fromDeg $ Deg.mk (-169.06666666622118)

example : DMS := ⟨90, 12, 0.999⟩

def checkShow (res : String) (tst : Unit -> DMS)  (name := res):=
  let got := toString $ tst ()
  let msg := if (got == res) then s!"ok: {res}" else "failed!:\n expect: {res}\n gotten: {got}"
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
  let v := "90°12′0.9999″"
  checkShow v (fun _ => ⟨90, 12, 0.9999⟩)

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

/-- info: 359°0′0.″ -/
#guard_msgs in #eval ⟨-1, 0, 0.0⟩ |> normalizeDMS
/-- info: 359°0′0.″ -/
#guard_msgs in #eval normalizeDMS ∘ fromDeg $ Deg.mk (-1.0)
/-- info: 1°0′0.″ -/
#guard_msgs in #eval ⟨361, 0, 0.0⟩ |> normalizeDMS
/-- info: 1°0′0.″ -/
#guard_msgs in #eval normalizeDMS ∘ fromDeg $ Deg.mk (361.0)
/-- info: 359°59′0.″ -/
#guard_msgs in #eval ⟨0, -1, 0.0⟩ |> normalizeDMS
/-- info: 0°1′1.″ -/
#guard_msgs in #eval ⟨0, 0, 61.0⟩ |> normalizeDMS

/-- info: 1°0′60.″ -/
#guard_msgs in
 /- TODO: Should be 1°1′0.″ -/
#eval ⟨0, 61, 0.0⟩ |> normalizeDMS

/-- info: 1°0′60.″ -/
#guard_msgs in
 /- TODO: Should be 1°1′0.″ -/
#eval ⟨1, 0, 60.0⟩ |> normalizeDMS
