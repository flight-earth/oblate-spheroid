import Lake
open Lake DSL

require batteries from git
  "git@github.com:leanprover-community/batteries" @ "v4.35.0"

package «oblate-spheroid»

lean_lib LatLng
lean_lib Earth
lean_lib Geodesy
lean_lib Units

@[default_target]
lean_exe «oblate-spheroid» where
  root := `Main
