namespace Units.Angle

class Angle (α : Type u) where
  /-- 0 <= a <= 2π -/
  normalize : α -> α

  /-- -π <= a <= +π -/
  plusMinusPi : α -> α

  /-- -π/2 <= a <= +π/2 -/
  plusMinusHalfPi : α -> Option α

  /-- rotation from an initial angle -/
  rotate : α -> α -> α
