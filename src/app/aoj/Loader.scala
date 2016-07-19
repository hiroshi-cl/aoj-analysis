package aoj

import scala.io.Source

object Loader {
  def load(): (Map[String, Int], Map[String, Int], Map[Int, String], Map[Int, String], Seq[Seq[Int]], Seq[Seq[Int]]) = {
    var user: Map[String, Int] = Map.empty
    var prob: Map[String, Int] = Map.empty
    var userId = 0
    var probId = 0
    val tbl = for (Seq(_, uid, probs) <- Source.fromFile("aoj.txt").getLines().grouped(3).toList) yield {
      user += uid -> userId
      userId += 1
      for (pid <- probs.split(' ').toSeq) yield
        prob.getOrElse(pid, {
          prob += pid -> probId
          val i = probId
          probId += 1
          i
        })
    }
    var tr: Seq[Seq[Int]] = Vector.fill[Vector[Int]](probId)(Vector.empty[Int])
    for ((ps, u) <- tbl.zipWithIndex; p <- ps)
      tr = tr.updated(p, tr(p) :+ u)

    var userInv: Map[Int, String] = Map.empty
    for ((name, num) <- user)
      userInv += num -> name
    var probInv: Map[Int, String] = Map.empty
    for ((name, num) <- prob)
      probInv += num -> name

    (user, prob, userInv, probInv, tbl, tr)
  }

  def loadProblemNames(): Seq[Problem] =
    Source.fromFile("probid.txt").getLines().map(_.split('\t')).map(ws => Problem(ws(0), ws(1))).toSeq

  def main(args: Array[String]): Unit = {
    val (_, _, _, _, tbl, tr) = load()
    println(tbl.mkString("\n"))
    println(tbl.map(_.size).sum)
  }
}
