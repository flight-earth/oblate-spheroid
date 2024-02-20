import Units

namespace Earth.Ellipsoid

open Units.Radius

structure Ellipsoid where
  equatorialR : Radius
  recipF : Float

instance : ToString Ellipsoid where
  toString e :=
    let r := e.equatorialR.radius
    "R=" ++ r.toString ++ ", 1/ƒ=" ++ e.recipF.toString

def flattening (e: Ellipsoid) : Float := 1.0 / e.recipF
def polarRadius (e: Ellipsoid ) : Radius := ⟨e.equatorialR.radius * (1.0 - flattening e)⟩

-- SEE: https:--en.wikipedia.org/wiki/World_Geodetic_System
-- https:--en.wikipedia.org/wiki/World_Geodetic_System#A_new_World_Geodetic_System:_WGS_84
def wgs84 : Ellipsoid := {equatorialR := ⟨6378137.0⟩, recipF := 298.257223563}

-- As used by the National Geodetic Survey tool inverse when selecting the
-- ellipsoid 1) GRS80 / WGS84 (NAD83) SEE:
-- https:--www.ngs.noaa.gov/PC_PROD/Inv_Fwd/
def nad83 : Ellipsoid := {wgs84 with recipF := 298.25722210088}

-- The Bessel ellipsoid from Vincenty 1975. Note that the flattening from
-- Wikipedia for the Bessel ellipsoid is 299.1528153513233 not 299.1528128. SEE:
-- https:--en.wikipedia.org/wiki/Bessel_ellipsoid
def bessel : Ellipsoid := {equatorialR := ⟨6377397.155⟩, recipF := 299.1528128}

-- The International ellipsoid 1924 also known as the Hayford ellipsoid from
-- Vincenty 1975. SEE: https:--en.wikipedia.org/wiki/Hayford_ellipsoid
def hayford : Ellipsoid := {equatorialR := ⟨6378388.0⟩, recipF := 297.0}

-- Clarke's 1866 ellipsoid approximated in metres. "Clarke actually defined his
-- 1866 spheroid as a = 20,926,062 British feet, b = 20,855,121 British feet"
-- SEE: https:--en.wikipedia.org/wiki/North_American_Datum
def clarke : Ellipsoid := {equatorialR := ⟨6378206.4⟩, recipF := 294.978698214}

-- The ellipsoid used in Evaluation Direct and Inverse Geodetic Algorithms, by
-- Paul Delorme, Bedford Institute of Oceanography, Dartmouth, Nova Scotia,
-- Canada, 1978.
def bedfordClarke := {clarke with recipF := 294.9786986}
