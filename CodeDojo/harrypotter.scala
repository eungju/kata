package harrypotter

sealed trait Book
case object First extends Book
case object Second extends Book
case object Third extends Book
case object Forth extends Book
case object Fifth extends Book

case class Bundle(books: Set[Book]) {
  val size = books.size
  val discountRate = Array(0, 0, 0.05, 0.1, 0.2, 0.25)(books.size)
  val price: Double = books.size * 8 * (1 - discountRate)
  def contains(book: Book): Boolean = books.contains(book)
  def add(book: Book): Bundle = Bundle(books + book)
}

case object Bundle {
  def apply(books: Book*): Bundle = Bundle(books.toSet)
}

case class Bundling(bundles: List[Bundle]) {
  val price: Double = bundles.foldLeft(0.0) { _ + _.price }
  def pack(book: Book): Set[Bundling] = {
    bundles.toSet.filter(!_.contains(book)).map((bundle) => {
      val (left, right) = bundles.span(_ != bundle)
      Bundling(left ++ right.drop(1) :+ bundle.add(book))
    }) + Bundling(bundles :+ Bundle(Set(book)))
  }
  def packAll(books: List[Book]): Set[Bundling] = {
    books.foldLeft(Set(this)) { (orders, book) =>
      val ub = orders.map(_.price).min + Bundle(book).price
      orders.flatMap(_.pack(book).filter(ub >= _.price))
    }
  }
}

case object Bundling {
  def apply(bundles: Bundle*): Bundling = Bundling(bundles.toList)
}

case class Order(books: Book*) {
  def price(): Double = Bundling().packAll(books.toList).map(_.price).min
}


import org.specs2.mutable._

class HarryPotterSpec extends Specification {
  "Bundle" should {
    val dut = Bundle(First)
    "know it's members" in {
      dut.contains(First) && !dut.contains(Second)
    }
    "know the number of books" in {
      dut.size must_== 1
    }
    "create another bundle with a new book" in {
      dut.add(Second) must_== Bundle(First, Second)
    }
  }

  "Bundle discount" should {
    "0% for one book" in {
      Bundle(First).price must_== 8.0
    }
    "5% for two books" in {
      Bundle(First, Second).price must_== 8.0 * 2 * 0.95
    }
    "10% for three books" in {
      Bundle(First, Second, Third).price must_== 8.0 * 3 * 0.90
    }
    "20% for four books" in {
      Bundle(First, Second, Third, Forth).price must_== 8.0 * 4 * 0.80
    }
    "25% for five books" in {
      Bundle(First, Second, Third, Forth, Fifth).price must_== 8.0 * 5 * 0.75
    }
  }

  "Bundling" should {
    "add new bundle if it has no bundle" in {
      Bundling().pack(First) must_== Set(Bundling(Bundle(First)))
    }
    "add new bundle if existing bundles can't accept the book" in {
      Bundling(Bundle(First)).pack(First) must_== Set(Bundling(Bundle(First), Bundle(First)))
    }
    "add the book into one of existing bundles or add new bundle if existing bundles can accept the book" in {
      Bundling(Bundle(First)).pack(Second) must_== Set(Bundling(Bundle(First, Second)), Bundling(Bundle(First), Bundle(Second)))
    }
    "create all promising possibilities" in {
      Bundling().packAll(List(First, Second)) must_== Set(Bundling(Bundle(First, Second)), Bundling(Bundle(First), Bundle(Second)))
    }
  }

  "Order discount" should {
    "be applied to 2-book bundle and 1-book bundle" in {
      Order(First, First, Second).price must_== (8 * 2 * 0.95) + 8
    }
    "be applied to two 2-book bundles" in {
      Order(First, First, Second, Second).price must_== 2 * (8 * 2 * 0.95)
    }
    "be applied to 4-book bundle and 2-book bundle. it's cheaper than two 3-book bundles" in {
      Order(First, First, Second, Third, Third, Forth).price must_== (8 * 4 * 0.8) + (8 * 2 * 0.95)
    }
    "be applied to 5-book bundle and 1-book bundle. it's cheapter than 4-book bundle and 2-book bundle" in {
      Order(First, Second, Second, Third, Forth, Fifth).price must_== (8 * 5 * 0.75) + 8
    }
    "be applied to two 4-book bundles. it's cheaper than 5-book bundle and 3-book bundle" in {
      Order(First, First, Second, Second, Third, Third, Forth, Fifth).price must_== 2 * (4 * 8 * 0.80)
    }
    "be applied to three 5-book bundles and 2 4-book bundles" in {
      Order(First, First, First, First, First,
        Second, Second, Second, Second, Second,
        Third, Third, Third, Third,
        Forth, Forth, Forth, Forth, Forth,
        Fifth, Fifth, Fifth, Fifth).price must_== 3 * (8 * 5 * 0.75) + 2 * (8 * 4 * 0.8)
    }
  }
}
