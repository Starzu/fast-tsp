package pl.edu.agh.fastaco.utils

class IdxWithValue(val cityIdx: Int, val value: Double)
object IdxWithValue {
  implicit val ordering: Ordering[IdxWithValue] =
    (x: IdxWithValue, y: IdxWithValue) =>
      if (x.value < y.value) -1
      else if (x.value > y.value) 1
      else 0
}
