package akovalevsky.scalabootcamp.homework

object DataStructures {
  def allEqual[T](list: List[T]): Boolean = {
    if (list.isEmpty)
      false
    else
      list.tail.forall(_.equals(list.head))
  }

  // Exercise. Calculate the total cost of all vegetables, taking vegetable amounts (in units) from
  // `vegetableAmounts` and prices per unit from `vegetablePrices`. Assume the price is 10 if not available
  // in `vegetablePrices`.

  val vegetableAmounts = Map(
    "tomatoes" -> 17,
    "peppers" -> 234,
    "olives" -> 32,
    "cucumbers" -> 323,
  )

  val vegetablePrices = Map(
    "tomatoes" -> 4,
    "peppers" -> 5,
    "olives" -> 17,
  )

  val totalVegetableCost: Int =
    vegetableAmounts.map { case (veg, amount) => vegetablePrices.getOrElse(veg, 10) * amount }.sum

  // Exercise. Given the vegetable weights (per 1 unit of vegetable) in `vegetableWeights` and vegetable
  // amounts (in units) in `vegetableAmounts`, calculate the total weight per type of vegetable, if known.
  //
  // For example, the total weight of "olives" is 2 * 32 == 64.

  val vegetableWeights = Map(
    ("pumpkins", 10),
    ("cucumbers", 20),
    ("olives", 2),
  )

  val totalVegetableWeights: Map[String, Int] =
    vegetableAmounts.keys.toList.intersect(vegetableWeights.keys.toList)
      .map(veg => (veg, vegetableWeights(veg) * vegetableAmounts(veg))).toMap

  def allSubsetsOfSizeN[A](set: Set[A], n: Int): Set[Set[A]] = set.subsets(n).toSet

  def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] =
    map
      .groupBy{case (_, value) => value}
      .toList
      .sortBy{case (value, _) => value}
      .map{case (value, keyValues) => (keyValues.map{case (key, _) => key}.toSet, value)}
}
