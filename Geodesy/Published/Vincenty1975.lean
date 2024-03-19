import Earth.Ellipsoid
import Units.DMS
open Earth.Ellipsoid
open Units.DMS


namespace Geodesy.Published.Vincenty1975

def ellipsoids : List Ellipsoid :=
  [ bessel, hayford, hayford, hayford, hayford ]

-- Distances in meters
def distances : List Float :=
    [     14110526.170
    ,      4085966.703
    ,      8084823.839
    ,     19960000.000
    ,     19780006.558
    ]

def xAzimuths : List DMS :=
    [ ⟨ 96, 36,  8.79960⟩
    , ⟨ 95, 27, 59.63089⟩
    , ⟨ 15, 44, 23.74850⟩
    , ⟨ 89,  0,  0.0⟩
    , ⟨  4, 59, 59.99995⟩
    ]

def yAzimuths : List DMS :=
    [ ⟨ 137, 52, 22.01454⟩
    , ⟨ 118,  5, 58.96161⟩
    , ⟨ 144, 55, 39.92147⟩
    , ⟨  91,  0,  6.11733⟩
    , ⟨ 174, 59, 59.88481⟩
    ]
