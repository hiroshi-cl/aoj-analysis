package aoj

import javax.inject.Inject

trait Service {
  val num = 5

  def userRank: Seq[Rank]

  def problemRank: Seq[Rank]

  def recommend(userId: String): Recommend

  def problemName(problemId: String): String
}

class RepositoryService @Inject()(repository: Repository) extends Service {
  def userRank: Seq[Rank] = repository.userRank

  def problemRank: Seq[Rank] = repository.problemRank

  private[this] var map: Map[String, Rank] = Map.empty
  for(rank <- userRank)
    map += rank.name -> rank

  private[this] var pmap: Map[String, String] = Map.empty
  for(problem <- repository.problemNames)
    pmap += problem.id -> problem.name

  def recommend(id: String): Recommend = {
    val myRank = map(id)
    val th = myRank.threshold.ceil.toInt
    val ranks = problemRank
    val easiers = ranks.drop(th).filterNot(prank => cacheRepository.solved(myRank.name, prank.name)).take(num).toList
    val moreDifficults = ranks.take(th).filterNot(prank => cacheRepository.solved(myRank.name, prank.name)).reverse.take(num).toList

    Recommend(myRank = myRank, easiers = easiers, moreDifficults = moreDifficults)
  }

  def problemName(problemId: String): String = pmap(problemId)
}

object ServiceMain {
  def main(args: Array[String]): Unit = {
    println(new RepositoryService(cacheRepository).recommend("no15_renne"))
    println(new RepositoryService(cacheRepository).recommend("eomole"))
    println(new RepositoryService(cacheRepository).recommend("tozangezan"))
    println(new RepositoryService(cacheRepository).problemName("0000"))
  }
}
