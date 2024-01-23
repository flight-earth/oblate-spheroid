import Std.Data.String.Basic
open String

structure DMS where
  deg : Int
  min : Int
  sec: Float
  deriving BEq

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

def checkEq {name : String} (res : DMS) (tst : Unit -> DMS) :=
  let got := tst ()
  let msg := if (got == res) then s!"ok: {res}" else "failed!:\n expect: {res}\n gotten: {got}"
  IO.println s!"eq {name}: {msg}"

def checkShow {name : String} (res : String) (tst : Unit -> DMS) :=
  let got := toString $ tst ()
  let msg := if (got == res) then s!"ok: {res}" else "failed!:\n expect: {res}\n gotten: {got}"
  IO.println s!"{name}: {msg}"

def testEq : IO Unit := do
  let v := DMS.mk 90 12 0.9999
  @checkEq (toString v) v (fun _ => ⟨90, 12, 0.9999⟩)

def testShow : IO Unit := do
  let v := "90°12′0.9999″"
  @checkShow v v (fun _ => ⟨90, 12, 0.9999⟩)
