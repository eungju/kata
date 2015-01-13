package manyprizes

import org.specs2.mutable._

case class ManyPrizes(n: Int) {
  //this doesn't provide enough precision
  def log(base: Int, x: Long):Int = (math.log(x) / math.log(base)).toInt
  def max_losts_for(p: Long): Int = {
    (0 to (n - 1)).map((i: Int) => {
      (i to (n - 1)).map(math.pow(2, _).toLong).sum
    }).count(_ < p)
    //val x = math.pow(2, n).toLong - p
    //if (x == 0) n else (n - log(2, x) - 1)
  }
  def max_number_for_lost(l: Int): Long = {
    math.min(math.pow(2, l + 1).toLong - 2, math.pow(2, n).toLong - 1)
  }
  def pessimistic_max_number_for(p: Long): Long = {
    max_number_for_lost(max_losts_for(p))
  }
  def min_wins_for(p: Long): Int = {
    (0 to (n - 1)).map((i: Int) => {
      (0 to i).map(math.pow(2, _).toLong).sum
    }).count(_ >= p)
    //n - log(2, p)
  }
  def max_number_for_win(w: Int): Long = {
    math.pow(2, n).toLong - math.pow(2, w).toLong
  }
  def optimistic_max_number_for(p: Long): Long = {
    max_number_for_win(min_wins_for(p))
  }
}

import io.StdIn.readLine

object ManyPrizes {
  def main(args: Array[String]): Unit = {
    val t = readLine().toInt
    (1 to t).foreach { i =>
      val tc: Array[String] = readLine().split(' ')
      val n = tc(0).toInt
      val p = tc(1).toLong
      val tournament = ManyPrizes(n)
      val max_p = tournament.pessimistic_max_number_for(p)
      val max_o = tournament.optimistic_max_number_for(p)
      println(s"Case #${i}: ${max_p} ${max_o}")
    }
  }
}

class ManyPrizesSpec extends Specification {
  "max_losts_for" should {
    val n3 = ManyPrizes(3)
    "0 for 1" in { n3.max_losts_for(1) must_== 0 }
    "0 for 4" in { n3.max_losts_for(4) must_== 0 }
    "1 for 5" in { n3.max_losts_for(5) must_== 1 }
    "1 for 6" in { n3.max_losts_for(6) must_== 1 }
    "2 for 7" in { n3.max_losts_for(7) must_== 2 }
    "3 for 8" in { n3.max_losts_for(8) must_== 3 }
  }
  "max_number_for_lost" should {
    val n3 = ManyPrizes(3)
    "0 for 0" in { n3.max_number_for_lost(0) must_== 0 }
    "2 for 1" in { n3.max_number_for_lost(1) must_== 2 }
    "6 for 2" in { n3.max_number_for_lost(2) must_== 6 }
    "7 for 3" in { n3.max_number_for_lost(3) must_== 7 }
  }
  "pessimistic_max_number_for" should {
    val n3 = ManyPrizes(3)
    "p=4" in {
      n3.pessimistic_max_number_for(4) must_== 0
    }
    "p=5" in {
      n3.pessimistic_max_number_for(5) must_== 2
    }
    "p=3" in {
      n3.pessimistic_max_number_for(3) must_== 0
    }
  }
  "min_wins_for" should {
    val n3 = ManyPrizes(3)
    "0 for 8" in { n3.min_wins_for(8) must_== 0 }
    "1 for 7" in { n3.min_wins_for(7) must_== 1 }
    "1 for 4" in { n3.min_wins_for(4) must_== 1 }
    "2 for 3" in { n3.min_wins_for(3) must_== 2 }
    "2 for 2" in { n3.min_wins_for(2) must_== 2 }
    "3 for 1" in { n3.min_wins_for(1) must_== 3 }
  }
  "max_number_for_win" should {
    val n3 = ManyPrizes(3)
    "0 for 3" in { n3.max_number_for_win(3) must_== 0 }
    "4 for 2" in { n3.max_number_for_win(2) must_== 4 }
    "6 for 1" in { n3.max_number_for_win(1) must_== 6 }
    "7 for 0" in { n3.max_number_for_win(0) must_== 7 }
  }
  "optimistic_max_number_for" should {
    val n3 = ManyPrizes(3)
    "p=4" in {
      ManyPrizes(3).optimistic_max_number_for(4) must_== 6
    }
    "p=5" in {
      ManyPrizes(3).optimistic_max_number_for(5) must_== 6
    }
    "p=3" in {
      ManyPrizes(3).optimistic_max_number_for(3) must_== 4
    }
  }
}
