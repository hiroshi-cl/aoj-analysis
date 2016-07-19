package aoj

// 0-based rank
case class Rank(rank: Int, name: String, threshold: Double)

case class Recommend(myRank: Rank, easiers: Seq[Rank], moreDifficults: Seq[Rank]) {
  override def toString: String = s"$myRank {\n\t$easiers\n\t$moreDifficults\n}"
}

case class Problem(id: String, name: String)
