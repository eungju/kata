package manyprizes

import org.specs2.mutable._

case class ManyPrizes(n: Int) {
  def max_losts_for(p: Long): Int = {
    ((n - 1) to 0 by -1).map((i: Int) => {
      ((n - 1) to i by -1) map (math.pow(2, _).toInt) sum
    }).count(_ < p)
  }
  def max_number_for_lost(l: Int): Int = {
    math.min(math.pow(2, l + 1).toInt - 2, math.pow(2, n).toInt - 1)
  }
  def max_number_for_granted(p: Int): Int = {
    max_number_for_lost(max_losts_for(p))
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
  "max_number_for_granted" should {
    val n3 = ManyPrizes(3)
    "p=4" in {
      ManyPrizes(3).max_number_for_granted(4) must_== 0
    }
    "p=5" in {
      ManyPrizes(3).max_number_for_granted(5) must_== 2
    }
    "p=3" in {
      ManyPrizes(3).max_number_for_granted(3) must_== 0
    }
  }
}
