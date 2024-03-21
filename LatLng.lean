import Units

namespace LatLng

open Units.DMS

structure LatLng where
  lat : Rad
  lng : Rad
  deriving BEq

instance : ToString LatLng where
  toString x :=
    let lat' := fromDeg $ fromRad x.lat
    let lng' := fromDeg $ fromRad x.lng
    toString (lat', lng')
