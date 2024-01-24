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

def fromDeg (α : Deg) : DMS :=
  let dAbs := α.deg.abs
  let dd : Int := dAbs.floor.toUInt64.toNat
  let dFrac := (dAbs - Float.ofInt dd)
  let mFrac := dFrac * 60.0
  let mins : Int := mFrac.floor.toUInt64.toNat
  let secs := (mFrac - Float.ofInt mins) * 60.0

  ⟨if signum α.deg == lt then Int.neg dd else dd, mins, secs⟩

def dropTrailingZeros (s : String) : String :=
  match Matcher.find? (Matcher.ofString ".") s with
  | none => s
  | some dps =>
    {dps with stopPos := String.endPos s}.dropRightWhile (· == '0')
    |> fun s' => s.take s'.startPos.byteIdx ++ toString s'

def display (x : DMS) : String :=
  toString x.deg ++ "°" ++
  toString x.min ++ "′" ++
  (dropTrailingZeros $ toString x.sec) ++ "″"

instance : ToString DMS where
  toString := display

example : DMS := ⟨90, 12, 0.999⟩

def checkShow (name : String) (res : String) (tst : Unit -> DMS) :=
  let got := toString $ tst ()
  let msg := if (got == res) then s!"ok: {res}" else "failed!:\n expect: {res}\n gotten: {got}"
  IO.println s!"{name}: {msg}"

def checkEq (name : String) (res : DMS) (tst : Unit -> DMS) :=
  let got := tst ()
  let msg := if (got == res) then s!"ok: {res}" else "failed!:\n expect: {res}\n gotten: {got}"
  IO.println s!"eq {name}: {msg}"

def checkFromDeg (name : String) (res : DMS) (tst : Unit -> DMS) :=
  let got := tst ()
  let msg := if (got == res) then s!"ok: {res}" else "failed!:\n expect: {res}\n gotten: {got}"
  IO.println s!"from-deg {name}: {msg}"

def testShow : IO Unit := do
  let v := "90°12′0.9999″"
  checkShow v v (fun _ => ⟨90, 12, 0.9999⟩)

def testEq : IO Unit := do
  let v := DMS.mk 90 12 0.9999
  checkEq (toString v) v (fun _ => ⟨90, 12, 0.9999⟩)

def testFromDeg : IO Unit := do
  let dmsZero := DMS.mk 0 0 0.0
  let dmsOne := DMS.mk 1 0 0.0
  let dmsMinusOne := DMS.mk (-1) 0 0.0
  let dms169 := DMS.mk 169 3 59.99999839625161
  let dmsMinus169 := DMS.mk (-169) 3 59.99999839625161
  checkFromDeg (toString dmsZero) (fromDeg $ Deg.mk 0.0) (fun _ => dmsZero)
  checkFromDeg (toString dmsOne) (fromDeg $ Deg.mk 1.0) (fun _ => dmsOne)
  checkFromDeg (toString dmsMinusOne) (fromDeg $ Deg.mk (-1.0)) (fun _ => dmsMinusOne)
  checkFromDeg (toString dms169) (fromDeg $ Deg.mk 169.06666666622118) (fun _ => dms169)
  checkFromDeg (toString dmsMinus169) (fromDeg $ Deg.mk (-169.06666666622118)) (fun _ => dmsMinus169)

#eval testFromDeg
#eval fromDeg $ Deg.mk (-169.06666666622118)
