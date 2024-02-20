import Std.Lean.Float

namespace Units.Convert

abbrev Deg : Type := Float
abbrev Rad : Type := Float
abbrev Min : Type := Float
abbrev Sec : Type := Float

def minToSec (m: Min): Sec := m * 60.0

def degToSec( d: Deg ): Sec := d * 3600.0

def degToMin( d: Deg ): Min := d * 60.0

def minToDeg( m: Min ): Deg := m / 60.0
def secToDeg( s: Sec ): Deg := s / 3600.0

-- SEE: https://doc.rust-lang.org/std/f64/consts/constant.PI.html
def pi := 3.14159265358979323846264338327950288

def degToRad( x: Deg ): Rad := x * pi / 180.0
def radToDeg( x: Rad) : Deg := x / pi * 180.0
