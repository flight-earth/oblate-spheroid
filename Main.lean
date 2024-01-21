import OblateSpheroid

def main : IO Unit := do
  let london := LatLng.mk 51.5007 (-0.1246)
  let newyork := LatLng.mk 40.6892 (-74.0445)
  IO.println s!"London: {london}"
  IO.println s!"New York: {newyork}"
