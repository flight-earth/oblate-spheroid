import Lake
open Lake DSL

require std from git
  "git@github.com:leanprover/std4/" @ "v4.6.0-rc1"

package «oblate-spheroid»

lean_lib Geodesy

lean_lib Units

@[default_target]
lean_exe «oblate-spheroid» where
  root := `Main
