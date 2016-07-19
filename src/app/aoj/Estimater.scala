package aoj

import java.io.PrintWriter

object Estimater {
  val powers = Stream.iterate(1.0)(_ * .5)

  def threasholdUser(prob: Array[Int], tbl: Seq[Seq[Int]]) = tbl.map(l =>
    l.map(prob).sorted.map(r => Math.log(r + 1)).zip(powers).map(p => p._1 * p._2).sum / powers.take(l.size).sum
  ).toArray

  def reorderUser(user: Array[Int], prob: Array[Int], tbl: Seq[Seq[Int]]): Int = {
    val min = threasholdUser(prob, tbl)
    var cnt = 0
    for ((id, rank) <- min.zipWithIndex.sorted.map(_._2).zipWithIndex if user(id) != rank) {
      cnt += 1
      user(id) = rank
    }
    cnt
  }

  def threasholdProb(user: Array[Int], tbl: Seq[Seq[Int]]) = tbl.map(l =>
    l.map(user).sorted(Ordering[Int].reverse).map(r => Math.log(r + 1)).zip(powers).map(p => p._1 * p._2).sum / powers.take(l.size).sum
  ).toArray

  def reorderProb(user: Array[Int], prob: Array[Int], tbl: Seq[Seq[Int]]): Int = {
    val max = threasholdProb(user, tbl)
    var cnt = 0
    for ((id, rank) <- max.zipWithIndex.sorted.map(_._2).zipWithIndex if prob(id) != rank) {
      cnt += 1
      prob(id) = rank
    }
    cnt
  }

  def converge(usize: Int, psize: Int, tbl: Seq[Seq[Int]], tr: Seq[Seq[Int]]): (Array[Int], Array[Double], Array[Int], Array[Double]) = {
    val user = 0.until(usize).toArray
    val prob = scala.util.Random.shuffle(0.until(psize).toList).toArray
    var step = 0
    import scala.util.control.Breaks._
    breakable {
      while (step < 20) {
        val updatedProb = reorderProb(user, prob, tr)
        val updatedUser = reorderUser(user, prob, tbl)
        println(s"$step:\t$updatedUser users and $updatedProb probs updated")
        if (updatedUser + updatedProb == 0)
          break
        step += 1
      }
    }

    println("converged!")

    val urank = Array.ofDim[Int](usize)
    for ((rank, id) <- user.zipWithIndex)
      urank(rank) = id

    val prank = Array.ofDim[Int](psize)
    for ((rank, id) <- prob.zipWithIndex)
      prank(rank) = id

    (urank, threasholdUser(prob, tbl), prank, threasholdProb(user, tr))
  }

  def ranks(rank: Seq[Int], name: Map[Int, String], th: Seq[Double]): Seq[Rank] =
    rank.zipWithIndex.map(pair => Rank(rank = pair._2, name = name(pair._1), threshold = Math.exp(th(pair._1)) - 1))

  def main(args: Array[String]): Unit = {
    val (_, _, u, p, tbl, tr) = Loader.load()
    val (urank, uth, prank, pth) = converge(u.size, p.size, tbl, tr)

    {
      val pw = new PrintWriter("result.txt")
      pw.println(urank.zipWithIndex.map(pair => f"${pair._2}: ${u(pair._1)} >= ${Math.exp(uth(pair._1)) - 1}%.1f").mkString("\n"))
      pw.println(prank.zipWithIndex.map(pair => f"${pair._2}: ${p(pair._1)} >= ${Math.exp(pth(pair._1)) - 1}%.1f").mkString("\n"))
      pw.flush()
      pw.close()
    }

    {
      val pw = new PrintWriter("user.txt")
      pw.println(urank.zipWithIndex.map(pair => f"${pair._2}\t${u(pair._1)}\t${Math.exp(uth(pair._1)) - 1}%.1f").mkString("\n"))
      pw.flush()
      pw.close()

    }

    {
      val pw = new PrintWriter("prob.txt")
      pw.println(prank.zipWithIndex.map(pair => f"${pair._2}\t${p(pair._1)}\t${Math.exp(pth(pair._1)) - 1}%.1f").mkString("\n"))
      pw.flush()
      pw.close()

    }
  }
}
