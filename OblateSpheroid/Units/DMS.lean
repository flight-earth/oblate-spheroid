structure DMS where
  deg : Int
  min : Int
  sec: Float

instance : ToString DMS where
  toString x :=
    toString x.deg ++ "°" ++
    toString x.min ++ "'" ++
    toString x.sec ++ "\""
