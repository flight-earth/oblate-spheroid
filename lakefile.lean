import Lake
open Lake DSL

package «oblate-spheroid» where
  -- add package configuration options here

lean_lib «OblateSpheroid» where
  -- add library configuration options here

@[default_target]
lean_exe «oblate-spheroid» where
  root := `Main
