import Lake
open Lake DSL

require std from git "git@github.com:leanprover/std4/" @ "main"

package «oblate-spheroid» where
  -- add package configuration options here

lean_lib «OblateSpheroid» where
  -- add library configuration options here

@[default_target]
lean_exe «oblate-spheroid» where
  root := `Main
