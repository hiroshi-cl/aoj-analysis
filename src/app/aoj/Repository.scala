package aoj

import scala.collection.BitSet
import scala.io.Source

trait Repository {
  def userRank: Seq[Rank]

  def problemRank: Seq[Rank]

  def solved(user: String, prob: String): Boolean

  def problemNames: Seq[Problem]
}

object realRepository extends Repository {
  private[this] val (uu, pp, u, p, tbl, tr) = Loader.load()
  private[this] val (urank, uth, prank, pth) = Estimater.converge(u.size, p.size, tbl, tr)
  lazy val userRank = Estimater.ranks(urank, u, uth)
  lazy val problemRank = Estimater.ranks(prank, p, pth)

  private[this] val solvedTable = tbl.map(BitSet(_: _*))
  def solved(user: String, prob: String): Boolean = solvedTable(uu(user))(pp(prob))
  lazy val problemNames = Loader.loadProblemNames()
}

object cacheRepository extends Repository {
  lazy val userRank = Source.fromFile("user.txt").getLines().map(_.split('\t')).map(ws => Rank(ws(0).toInt, ws(1), ws(2).toDouble)).toSeq
  lazy val problemRank = Source.fromFile("prob.txt").getLines().map(_.split('\t')).map(ws => Rank(ws(0).toInt, ws(1), ws(2).toDouble)).toSeq

  private[this] val (uu, pp, u, p, tbl, tr) = Loader.load()
  private[this] val solvedTable = tbl.map(BitSet(_: _*))
  def solved(user: String, prob: String): Boolean = solvedTable(uu(user))(pp(prob))
  lazy val problemNames = Loader.loadProblemNames()
}

object RepositoryMain {
  def main(args: Array[String]): Unit = {
    println(realRepository.userRank)
    println(realRepository.problemRank)
    println(cacheRepository.userRank)
    println(cacheRepository.problemRank)
    println(cacheRepository.userRank.map(urank => cacheRepository.problemRank.filterNot(prank => cacheRepository.solved(urank.name, prank.name)).toList).take(10).mkString("\n"))
  }
}
